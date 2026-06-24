open! Core
open! Bonsai_web
open! Js_of_ocaml

module Todos = Todo_core
module Screen = Todos.Screen
module Attr = Vdom.Attr
module Node = Vdom.Node

module Worker_store = struct
  module Json = Yojson.Safe
  module Json_util = Yojson.Safe.Util

  type response =
    { request_id : int
    ; action : Todos.Action.t
    }

  let next_request_id = ref 0
  let pending : (int, Todos.Action.t -> unit) Hashtbl.t = Hashtbl.create (module Int)

  let todo_to_json (todo : Todos.Todo.t) =
    `Assoc
      [ "id", `String todo.id
      ; "title", `String todo.title
      ; "completed", `Bool todo.completed
      ; "created_at_ms", `Int todo.created_at_ms
      ]
  ;;

  let write_to_json (write : Todos.Store_write.t) =
    match write with
    | Add todo -> `Assoc [ "type", `String "Add"; "todo", todo_to_json todo ]
    | Toggle id -> `Assoc [ "type", `String "Toggle"; "id", `String id ]
    | Delete id -> `Assoc [ "type", `String "Delete"; "id", `String id ]
  ;;

  let command_to_json ~request_id (command : Todos.Command.t) =
    match command.request with
    | Load_all | Subscribe_query _ | Unsubscribe_query _ ->
      `Assoc [ "requestId", `Int request_id; "type", `String "LoadAll" ]
    | Persist write ->
      `Assoc
        [ "requestId", `Int request_id
        ; "type", `String "Persist"
        ; "write", write_to_json write
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

  let response_of_json json =
    let open Json_util in
    let request_id = json |> member "requestId" |> to_int in
    let action =
      match json |> member "type" |> to_string with
      | "Loaded" ->
        json |> member "todos" |> to_list |> List.map ~f:todo_of_json |> Todos.Action.Loaded
      | "StoreFailed" -> Todos.Action.Store_failed (json |> member "message" |> to_string)
      | response_type ->
        Todos.Action.Store_failed [%string "Unknown worker response: %{response_type}"]
    in
    { request_id; action }
  ;;

  let decode_response payload =
    try response_of_json (Json.from_string payload) with
    | exn ->
      { request_id = -1
      ; action = Store_failed [%string "Invalid worker response: %{Exn.to_string exn}"]
      }
  ;;

  let worker =
    lazy
      (let worker : (Js.js_string Js.t, Js.js_string Js.t) Worker.worker Js.t =
         Worker.create "app/todos_db_worker.bc.js?v=1"
       in
       worker##.onmessage
       := Dom_html.handler (fun event ->
         let response = decode_response (Js.to_string event##.data) in
         (match Hashtbl.find pending response.request_id with
          | Some callback ->
            Hashtbl.remove pending response.request_id;
            callback response.action
          | None -> ());
         Js._false);
       worker)
  ;;

  let next_id () =
    incr next_request_id;
    !next_request_id
  ;;

  let effect command =
    Bonsai.Effect.Expert.of_fun ~f:(fun ~callback ~on_exn:_ ->
      let request_id = next_id () in
      Hashtbl.set pending ~key:request_id ~data:callback;
      let payload = command_to_json ~request_id command |> Json.to_string |> Js.string in
      (Lazy.force worker)##postMessage payload)
  ;;

  let run_command ~dispatch command =
    let open Bonsai.Effect.Let_syntax in
    let%bind action = effect command in
    dispatch action
  ;;
end

let empty_state text =
  Node.div
    ~attrs:[ Attr.class_ "empty-state" ]
    [ Node.h2 [ Node.text text ]; Node.p [ Node.text "Nothing here right now." ] ]
;;

let todo_row ~dispatch (todo : Todos.Todo.t) =
  let completed_class = if todo.completed then [ "completed" ] else [] in
  Node.li
    ~key:todo.id
    ~attrs:[ Attr.classes ("todo-row" :: completed_class) ]
    [ Node.button
        ~attrs:
          [ Attr.class_ "icon-button"
          ; Attr.title (if todo.completed then "Mark incomplete" else "Mark complete")
          ; Attr.on_click (fun _ -> dispatch (Todos.Action.Toggle todo.id))
          ]
        [ Node.text (if todo.completed then "✓" else "") ]
    ; Node.span [ Node.text todo.title ]
    ; Node.button
        ~attrs:
          [ Attr.class_ "delete-button"
          ; Attr.title "Delete"
          ; Attr.on_click (fun _ -> dispatch (Todos.Action.Delete todo.id))
          ]
        [ Node.text "Delete" ]
    ]
;;

let todo_list ~empty_title todos ~dispatch =
  match todos with
  | [] -> empty_state empty_title
  | todos -> Node.ul (List.map todos ~f:(todo_row ~dispatch))
;;

let view ({ model; dispatch } : Todos.Controller.t) =
  let screen =
    Screen.create
      model
      ~route:Screen.Route.All
      ~search:""
      ~selected_todo_id:""
  in
  Node.main
    ~attrs:[ Attr.class_ "app-shell" ]
    [ Node.aside
        ~attrs:[ Attr.class_ "sidebar" ]
        [ Node.h1 [ Node.text "Todos" ]
        ; Node.p
            ~attrs:[ Attr.class_ "counter" ]
            [ Node.text [%string "%{screen.active_count#Int} active"] ]
        ; Node.p
            ~attrs:[ Attr.classes [ "counter"; "muted" ] ]
            [ Node.text [%string "%{screen.completed_count#Int} completed"] ]
        ; (match model.error with
           | None -> Node.none
           | Some error -> Node.p ~attrs:[ Attr.class_ "error" ] [ Node.text error ])
        ]
    ; Node.section
        ~attrs:[ Attr.class_ "workspace" ]
        [ Node.form
            ~attrs:
              [ Attr.class_ "composer"
              ; Attr.on_submit (fun event ->
                  Dom.preventDefault event;
                  dispatch (Todos.Action.Submit_new (Screen.next_todo model)))
              ]
            [ Node.input
                ~attrs:
                  [ Attr.type_ "text"
                  ; Attr.placeholder "New task"
                  ; Attr.value_prop model.draft
                  ; Attr.on_input (fun _ draft -> dispatch (Todos.Action.Set_draft draft))
                  ]
                ()
            ; Node.button ~attrs:[ Attr.type_ "submit" ] [ Node.text "Add" ]
            ]
        ; Node.div
            ~attrs:[ Attr.class_ "columns" ]
            [ Node.section
                [ Node.h2 [ Node.text "Active" ]
                ; todo_list screen.active_todos ~empty_title:"All clear" ~dispatch
                ]
            ; Node.section
                [ Node.h2 [ Node.text "Done" ]
                ; todo_list screen.completed_todos ~empty_title:"No completed tasks" ~dispatch
                ]
            ]
        ]
    ]
;;

let component graph =
  let open Bonsai.Let_syntax in
  let controller = Todos.Controller.component ~run_command:Worker_store.run_command graph in
  let on_activate =
    let%arr controller in
    controller.dispatch Todos.Action.Load
  in
  Bonsai.Edge.lifecycle ~on_activate graph;
  let%arr controller in
  view controller
;;

let () = Bonsai_web.Start.start ~bind_to_element_with_id:"app" component
