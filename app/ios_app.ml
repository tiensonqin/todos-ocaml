open! Todo_std
module Swiftui = Bonsai_apple_swiftui
module Todos = Todos.Todo_runtime

let app = ref None
let window = ref None

let dispatch_on_main ~dispatch action =
  Swiftui.run_on_main (fun () ->
      dispatch action ();
      Option.iter !app ~f:Swiftui.App.flush_and_render)

let run_todos_command ~path ~dispatch (command : Todos.Command.t) =
  Swiftui.Apple.Action.of_thunk (fun () ->
      match command.request with
      | Load_page _ ->
          let run () =
            let result = Todos.Runtime.execute_command ~path command in
            dispatch_on_main ~dispatch result
          in
          ignore (Thread.create run ())
      | Persist _ ->
          let run () =
            match Todos.Runtime.execute_command ~path command with
            | Store_failed _ as result -> dispatch_on_main ~dispatch result
            | Loaded_page _ | Persisted _ -> ()
            | Load_page _ | Set_draft _ | Submit_new _ | Update_title _
            | Toggle _ | Delete _ ->
                ()
          in
          ignore (Thread.create run ()))

let install_root_view app_delegate _application _launch_options =
  let db_path = Todos.Runtime.default_db_path () in
  let swiftui_app =
    Swiftui.App.create
      (Todo_ui.adaptive_component
         ~run_command:(run_todos_command ~path:db_path))
  in
  Swiftui.App.flush_and_render swiftui_app;
  let root = Option.value_exn (Swiftui.App.view swiftui_app) in
  let root_window = Swiftui.window root in
  app := Some swiftui_app;
  window := Some root_window;
  ignore app_delegate;
  true

let () = Swiftui.run_application install_root_view
