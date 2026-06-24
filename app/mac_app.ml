open! Core

module Swiftui = Bonsai_apple_swiftui
module Todos = Todos.Todo_runtime

let app = ref None
let window = ref None

let dispatch_on_main ~dispatch action =
  Swiftui.run_on_main (fun () ->
    Bonsai.Effect.Expert.handle
      (dispatch action)
      ~on_exn:(fun exn -> Exn.reraise exn "Unhandled todos action");
    Option.iter !app ~f:Swiftui.App.flush_and_render)
;;

let run_todos_command ~path ~dispatch (command : Todos.Command.t) =
  Bonsai.Effect.Expert.of_fun ~f:(fun ~callback ~on_exn ->
    match command.request with
    | Subscribe_query { id; query } ->
      let is_initial = ref true in
      let handle_initial action =
        Bonsai.Effect.Expert.handle (dispatch action) ~on_exn
      in
      (try
         Todos.Runtime.subscribe_query
           ~path
           ~id
           ~query
           ~on_change:(fun action ->
             if !is_initial
             then (
               is_initial := false;
               handle_initial action)
             else dispatch_on_main ~dispatch action)
       with
       | exn -> handle_initial (Todos.Action.Store_failed (Exn.to_string exn)));
      callback ()
    | Unsubscribe_query id ->
      Todos.Runtime.unsubscribe_query ~path ~id;
      callback ()
    | Load_all | Persist _ ->
      let run () =
        let result =
          Todos.Runtime.execute_command ~notify_subscribers:false ~path command
        in
        dispatch_on_main ~dispatch result;
        callback ()
      in
      ignore (Caml_threads.Thread.create run ()))
;;

let install_root_view ~time_source _delegate _application _launch_options =
  let db_path = Todos.Runtime.default_db_path () in
  let swiftui_app =
    Swiftui.App.create
      ~time_source
      (Todo_ui.component ~run_command:(run_todos_command ~path:db_path))
  in
  app := Some swiftui_app;
  Swiftui.App.flush_and_render swiftui_app;
  let root = Option.value_exn (Swiftui.App.view swiftui_app) in
  let root_window = Swiftui.window root in
  window := Some root_window;
  true
;;

let () =
  Swiftui.run_application
    (install_root_view ~time_source:(Bonsai.Time_source.create ~start:Time_ns.epoch))
;;
