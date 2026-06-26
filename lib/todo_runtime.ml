open! Todo_std
include Todo_core

module Store = struct
  include Todo_core.Store

  let open_sqlite ~path =
    let sqlite = Datascript_sqlite.open_session path in
    let storage = Datascript_sqlite.storage sqlite in
    restore_or_create storage
end

module Runtime = struct
  type session = { path : string; store : Store.t ref; mutex : Mutex.t }

  let default_db_filename = "todos-ocaml.sqlite3"

  let default_db_path_for_env ~getenv =
    match getenv "BONSAI_TODOS_DB" with
    | Some path -> path
    | None -> (
        match getenv "HOME" with
        | Some home when not (String.is_empty home) ->
            Stdlib.Filename.concat
              (Stdlib.Filename.concat home "Documents")
              default_db_filename
        | _ ->
            Stdlib.Filename.concat
              (Stdlib.Filename.get_temp_dir_name ())
              default_db_filename)

  let default_db_path () = default_db_path_for_env ~getenv:Sys.getenv_opt
  let sessions : (string, session) Hashtbl.t = Hashtbl.create ()
  let sessions_mutex = Mutex.create ()

  let session ~path =
    Mutex.lock sessions_mutex;
    let result =
      match Hashtbl.find sessions path with
      | Some session -> session
      | None ->
          let session =
            {
              path;
              store = ref (Store.open_sqlite ~path);
              mutex = Mutex.create ();
            }
          in
          Hashtbl.set sessions ~key:path ~data:session;
          session
    in
    Mutex.unlock sessions_mutex;
    result

  let with_session session ~f =
    Mutex.lock session.mutex;
    try
      let result = f !(session.store) in
      Mutex.unlock session.mutex;
      result
    with exn ->
      Mutex.unlock session.mutex;
      raise exn

  let execute_command_with_session session (command : Command.t) =
    with_session session ~f:(fun store ->
        match command.request with
        | Load_page { limit; offset; search } ->
            let todos, has_more =
              Store.title_search_page store ~limit ~offset ~search
            in
            Action.Loaded_page { todos; has_more; offset; search }
        | Persist write ->
            let store = Store.apply_write store write in
            session.store := store;
            Action.Persisted write)

  let execute_command ~path command =
    try execute_command_with_session (session ~path) command
    with exn -> Action.Store_failed (Exn.to_string exn)

  let action ~path command () =
    let session = session ~path in
    try execute_command_with_session session command
    with exn -> Action.Store_failed (Exn.to_string exn)

  let run_command ~path ~dispatch command =
    match command.Command.request with
    | Load_page _ | Persist _ ->
        Bonsai_native.Action.of_thunk (fun () ->
            let run () = dispatch (action ~path command ()) () in
            ignore (Thread.create run ()))
end
