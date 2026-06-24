open! Core
open! Js_of_ocaml

module Todos = Todos.Todo_core
module Json = Yojson.Safe
module Json_util = Yojson.Safe.Util

let global = Js.Unsafe.global

let post_message text =
  Js.Unsafe.fun_call
    (Js.Unsafe.get global "postMessage")
    [| Js.Unsafe.inject (Js.string text) |]
;;

let import_scripts path =
  ignore
    (Js.Unsafe.fun_call
       (Js.Unsafe.get global "importScripts")
       [| Js.Unsafe.inject (Js.string path) |]
     : Js.Unsafe.any)
;;

let initialize_sqlite_wasm () =
  import_scripts "../vendor/sqlite-wasm/sqlite3.js";
  let sqlite_dir =
    Js.Unsafe.fun_call
      (Js.Unsafe.js_expr
         "(function(){ return new URL('../vendor/sqlite-wasm/', self.location.href).href; })")
      [||]
  in
  Js.Unsafe.set
    (Js.Unsafe.get global "sqlite3InitModuleState")
    "sqlite3Dir"
    sqlite_dir
;;

module Sqlite = struct
  let db_filename = "/todos-ocaml.sqlite3"

  let is_nullish value =
    Js.to_bool
      (Js.Unsafe.fun_call
         (Js.Unsafe.js_expr "(function(value){ return value == null; })")
         [| Js.Unsafe.inject value |])
  ;;

  let get_string row field =
    let value = Js.Unsafe.get row field in
    if is_nullish value then None else Some (Js.to_string value)
  ;;

  let exec db sql =
    ignore
      (Js.Unsafe.meth_call db "exec" [| Js.Unsafe.inject (Js.string sql) |]
       : Js.Unsafe.any)
  ;;

  let exec_with_options db options =
    ignore (Js.Unsafe.meth_call db "exec" [| Js.Unsafe.inject options |] : Js.Unsafe.any)
  ;;

  let bind values = Js.array (Array.of_list values)

  let option ?bind_values ?row_mode ?callback sql =
    let options = Js.Unsafe.obj [||] in
    Js.Unsafe.set options "sql" (Js.string sql);
    Option.iter bind_values ~f:(fun values -> Js.Unsafe.set options "bind" (bind values));
    Option.iter row_mode ~f:(fun row_mode ->
      Js.Unsafe.set options "rowMode" (Js.string row_mode));
    Option.iter callback ~f:(fun callback ->
      Js.Unsafe.set options "callback" (Js.wrap_callback callback));
    options
  ;;

  type t =
    { db : Js.Unsafe.any }

  let create db_constructor =
    let db =
      Js.Unsafe.new_obj
        db_constructor
        [| Js.Unsafe.inject (Js.string db_filename) |]
    in
    exec
      db
      {|
        create table if not exists todos_ocaml_kv (
          address text primary key not null,
          payload text not null
        );
      |};
    { db }
  ;;

  let storage_store t entries =
    let db = t.db in
    exec db "begin immediate;";
    try
      List.iter entries ~f:(fun (address, payload) ->
        exec_with_options
          db
          (option
             "insert or replace into todos_ocaml_kv (address, payload) values (?, ?);"
             ~bind_values:
               [ Js.Unsafe.inject (Js.string address)
               ; Js.Unsafe.inject (Js.string (Todos.Storage_codec.encode payload))
               ]));
      exec db "commit;"
    with
    | exn ->
      exec db "rollback;";
      raise exn
  ;;

  let storage_restore t address =
    let db = t.db in
    let found = ref None in
    exec_with_options
      db
      (option
         "select payload from todos_ocaml_kv where address = ? limit 1;"
         ~bind_values:[ Js.Unsafe.inject (Js.string address) ]
         ~row_mode:"object"
         ~callback:(fun row ->
           found := Option.map (get_string row "payload") ~f:Todos.Storage_codec.decode));
    !found
  ;;

  let storage_list_addresses t =
    let db = t.db in
    let addresses = ref [] in
    exec_with_options
      db
      (option
         "select address from todos_ocaml_kv order by address;"
         ~row_mode:"object"
         ~callback:(fun row ->
           match get_string row "address" with
           | Some address -> addresses := address :: !addresses
           | None -> ()));
    List.rev !addresses
  ;;

  let storage_delete t addresses =
    let db = t.db in
    exec db "begin immediate;";
    try
      List.iter addresses ~f:(fun address ->
        exec_with_options
          db
          (option
             "delete from todos_ocaml_kv where address = ?;"
             ~bind_values:[ Js.Unsafe.inject (Js.string address) ]));
      exec db "commit;"
    with
    | exn ->
      exec db "rollback;";
      raise exn
  ;;

  let storage t : Todos.Store.Ds.storage =
    { storage_store = storage_store t
    ; storage_restore = storage_restore t
    ; storage_list_addresses = (fun () -> storage_list_addresses t)
    ; storage_delete = storage_delete t
    }
  ;;
end

let todo_to_json (todo : Todos.Todo.t) =
  `Assoc
    [ "id", `String todo.id
    ; "title", `String todo.title
    ; "completed", `Bool todo.completed
    ; "created_at_ms", `Int todo.created_at_ms
    ]
;;

let action_to_json ~request_id = function
  | Todos.Action.Loaded todos ->
    `Assoc
      [ "requestId", `Int request_id
      ; "type", `String "Loaded"
      ; "todos", `List (List.map todos ~f:todo_to_json)
      ]
  | Store_failed message ->
    `Assoc
      [ "requestId", `Int request_id
      ; "type", `String "StoreFailed"
      ; "message", `String message
      ]
  | Load
  | Set_draft _
  | Submit_new _
  | Toggle _
  | Delete _
  | Subscribe_query _
  | Unsubscribe_query _ ->
    `Assoc
      [ "requestId", `Int request_id
      ; "type", `String "StoreFailed"
      ; "message", `String "Worker cannot emit UI-only action"
      ]
;;

let todo_of_json json : Todos.Todo.t =
  let open Json_util in
  { id = json |> member "id" |> to_string
  ; title = json |> member "title" |> to_string
  ; completed = json |> member "completed" |> to_bool
  ; created_at_ms = json |> member "created_at_ms" |> to_int
  }
;;

let write_of_json json : Todos.Store_write.t =
  let open Json_util in
  match json |> member "type" |> to_string with
  | "Add" -> Add (todo_of_json (json |> member "todo"))
  | "Toggle" -> Toggle (json |> member "id" |> to_string)
  | "Delete" -> Delete (json |> member "id" |> to_string)
  | write_type -> failwithf "Unknown write: %s" write_type ()
;;

let command_of_json json : int * Todos.Command.t =
  let open Json_util in
  let request_id = json |> member "requestId" |> to_int in
  let request =
    match json |> member "type" |> to_string with
    | "LoadAll" -> Todos.Command.Load_all
    | "Persist" -> Persist (write_of_json (json |> member "write"))
    | request_type -> failwithf "Unknown request: %s" request_type ()
  in
  request_id, { Todos.Command.target = Background; request }
;;

type worker_state =
  | Ready of
      { storage : Todos.Store.Ds.storage
      ; store : Todos.Store.t ref
      }
  | Failed_to_restore of string

let execute_command state (command : Todos.Command.t) =
  match state with
  | Failed_to_restore message -> Todos.Action.Store_failed message
  | Ready { storage; store } ->
    (match command.request with
     | Load_all | Subscribe_query _ | Unsubscribe_query _ ->
       store := Todos.Store.restore_or_create storage;
       Todos.Action.Loaded (Todos.Store.list !store)
     | Persist write ->
       let _stored = Todos.Store.apply_write !store write in
       store := Todos.Store.restore_or_create storage;
       Loaded (Todos.Store.list !store))
;;

let state = ref None
let pending_payloads = Queue.create ()

let process_payload state payload : unit =
  let request_id, action =
    try
      let request_id, command = command_of_json (Json.from_string payload) in
      request_id, execute_command state command
    with
    | exn -> -1, Todos.Action.Store_failed (Exn.to_string exn)
  in
  post_message (action_to_json ~request_id action |> Json.to_string)
;;

let handle_payload payload : unit =
  match !state with
  | None -> Queue.enqueue pending_payloads payload
  | Some state -> process_payload state payload
;;

let install_message_handler () =
  Js.Unsafe.set
    global
    "onmessage"
    (Js.wrap_callback (fun event ->
       let payload = Js.Unsafe.get event "data" |> Js.to_string in
       handle_payload payload))
;;

let set_state ready_state =
  state := Some ready_state;
  while not (Queue.is_empty pending_payloads) do
    let payload = Queue.dequeue_exn pending_payloads in
    process_payload ready_state payload
  done
;;

let start_with_db_constructor db_constructor =
  let db = Sqlite.create db_constructor in
  let storage = Sqlite.storage db in
  let ready_state =
    match
      Result.try_with (fun () -> Todos.Store.restore_or_create storage)
    with
    | Ok store -> Ready { storage; store = ref store }
    | Error exn -> Failed_to_restore (Exn.to_string exn)
  in
  set_state ready_state
;;

let start sqlite3 =
  let options =
    Js.Unsafe.obj
      [| "name", Js.Unsafe.inject (Js.string "todos-ocaml-opfs")
       ; "initialCapacity", Js.Unsafe.inject 20
      |]
  in
  let promise =
    Js.Unsafe.meth_call
      sqlite3
      "installOpfsSAHPoolVfs"
      [| Js.Unsafe.inject options |]
  in
  ignore
    (Js.Unsafe.meth_call
       promise
       "then"
       [| Js.Unsafe.inject
            (Js.wrap_callback (fun pool_util ->
               let db_constructor = Js.Unsafe.get pool_util "OpfsSAHPoolDb" in
               start_with_db_constructor db_constructor))
        ; Js.Unsafe.inject
            (Js.wrap_callback (fun error ->
               set_state (Failed_to_restore (Js.to_string (Js.Unsafe.coerce error)))))
       |]
     : Js.Unsafe.any)
;;

let () =
  install_message_handler ();
  initialize_sqlite_wasm ();
  let promise =
    Js.Unsafe.fun_call (Js.Unsafe.get global "sqlite3InitModule") [||]
  in
  ignore
    (Js.Unsafe.meth_call
       promise
       "then"
       [| Js.Unsafe.inject (Js.wrap_callback start) |]
     : Js.Unsafe.any)
;;
