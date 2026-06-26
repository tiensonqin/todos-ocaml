module Transit_json = Transit.Json

type renderer

external create_todo_renderer :
  string ->
  (unit -> string) ->
  (string -> unit) ->
  (unit -> unit) ->
  (int -> unit) ->
  (int -> unit) ->
  renderer = "createTodoRenderer"
[@@mel.module "./react_runtime.js"]

external render : renderer -> unit = "render" [@@mel.send]

type todo_store

external create_todo_store : (string -> unit) -> todo_store = "createTodoStore"
[@@mel.module "./db_worker_client.js"]

external post_store_message : todo_store -> string -> unit = "post" [@@mel.send]

type todo = { id : int; title : string; completed : bool }

let todos = ref []
let draft = ref ""
let renderer_ref : renderer option ref = ref None

let todo_to_transit todo =
  Transit_json.Map
    [
      (Transit_json.Keyword "todo/id", Transit_json.Int todo.id);
      (Transit_json.Keyword "todo/title", Transit_json.String todo.title);
      (Transit_json.Keyword "todo/completed", Transit_json.Bool todo.completed);
    ]

let field key entries =
  List.find_map
    (fun (entry_key, value) ->
      match entry_key with
      | Transit_json.Keyword entry_key when String.equal entry_key key ->
          Some value
      | Transit_json.String entry_key when String.equal entry_key key ->
          Some value
      | _ -> None)
    entries

let int_field key entries =
  match field key entries with
  | Some (Transit_json.Int value) -> Some value
  | Some (Transit_json.Int64 value) -> Some (Int64.to_int value)
  | _ -> None

let string_field key entries =
  match field key entries with
  | Some (Transit_json.String value) -> Some value
  | _ -> None

let bool_field key entries =
  match field key entries with
  | Some (Transit_json.Bool value) -> Some value
  | _ -> None

let todo_of_transit = function
  | Transit_json.Map entries -> (
      match
        (int_field "todo/id" entries, string_field "todo/title" entries)
      with
      | Some id, Some title ->
          Some
            {
              id;
              title;
              completed =
                (match bool_field "todo/completed" entries with
                | Some completed -> completed
                | None -> false);
            }
      | _ -> None)
  | _ -> None

let todos_to_transit todos = Transit_json.Array (List.map todo_to_transit todos)

let todos_of_transit = function
  | Transit_json.Array values | Transit_json.List values ->
      List.filter_map todo_of_transit values
  | _ -> []

let encode_todos todos = Transit_json.to_string (todos_to_transit todos)

let decode_todos payload =
  if String.equal payload "" then []
  else try payload |> Transit_json.of_string |> todos_of_transit with _ -> []

let rerender () =
  match !renderer_ref with None -> () | Some renderer -> render renderer

let handle_store_message message =
  if String.starts_with ~prefix:"loaded:" message then (
    let payload = String.sub message 7 (String.length message - 7) in
    todos := decode_todos payload;
    rerender ())
  else if String.starts_with ~prefix:"failed:" message then rerender ()
  else ()

let store = create_todo_store handle_store_message
let persist_todos value = post_store_message store ("save:" ^ encode_todos value)
let load_todos () = post_store_message store "load"

let next_id todos =
  todos |> List.map (fun todo -> todo.id) |> List.fold_left max 0 |> ( + ) 1

let active_todos todos = List.filter (fun todo -> not todo.completed) todos
let completed_todos todos = List.filter (fun todo -> todo.completed) todos

let json_escape text =
  let buffer = Buffer.create (String.length text + 8) in
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | char -> Buffer.add_char buffer char)
    text;
  Buffer.contents buffer

let json_string text = "\"" ^ json_escape text ^ "\""

let todo_to_json todo =
  Printf.sprintf {|{"id":%d,"title":%s,"completed":%s}|} todo.id
    (json_string todo.title)
    (if todo.completed then "true" else "false")

let todos_to_json todos =
  todos |> List.map todo_to_json |> String.concat "," |> Printf.sprintf "[%s]"

let state_json () =
  let active = active_todos !todos in
  let completed = completed_todos !todos in
  Printf.sprintf
    {|{"draft":%s,"activeCount":%d,"completedCount":%d,"activeTodos":%s,"completedTodos":%s}|}
    (json_string !draft) (List.length active) (List.length completed)
    (todos_to_json active) (todos_to_json completed)

let set_draft value =
  draft := value;
  rerender ()

let add_todo () =
  let title = String.trim !draft in
  if not (String.equal title "") then (
    let updated =
      { id = next_id !todos; title; completed = false } :: !todos
    in
    todos := updated;
    draft := "";
    persist_todos updated;
    rerender ())

let toggle_todo id =
  let updated =
    !todos
    |> List.map (fun todo ->
           if todo.id = id then { todo with completed = not todo.completed }
           else todo)
  in
  todos := updated;
  persist_todos updated;
  rerender ()

let delete_todo id =
  let updated = !todos |> List.filter (fun todo -> todo.id <> id) in
  todos := updated;
  persist_todos updated;
  rerender ()

let renderer =
  create_todo_renderer "app" state_json set_draft add_todo toggle_todo
    delete_todo

let () =
  renderer_ref := Some renderer;
  render renderer;
  load_todos ()
