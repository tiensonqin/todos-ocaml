module Native = Bonsai_native
module App = Native.App

type renderer

external create_renderer :
  string ->
  (unit -> string) ->
  (int -> unit) ->
  (int -> string -> unit) ->
  renderer = "createRenderer"
[@@mel.module "./react_runtime.js"]

external render : renderer -> unit = "render" [@@mel.send]

type todo = { id : int; title : string; completed : bool }

let active_todos todos = List.filter (fun todo -> not todo.completed) todos
let completed_todos todos = List.filter (fun todo -> todo.completed) todos

let todo_row graph ~set_todos todos todo =
  let toggle () =
    todos
    |> List.map (fun current ->
        if current.id = todo.id then
          { current with completed = not current.completed }
        else current)
    |> set_todos
    |> fun action -> action ()
  in
  let delete () =
    todos |> List.filter (fun current -> current.id <> todo.id) |> set_todos
    |> fun action -> action ()
  in
  Native.scope graph ~key:(string_of_int todo.id) (fun _graph ->
      Native.hstack ~spacing:8.
        [
          Native.button
            (if todo.completed then "Undo" else "Done")
            ~on_click:toggle;
          Native.text todo.title;
          Native.button "Delete" ~on_click:delete;
        ])

let todo_list graph ~title todos ~set_todos all_todos =
  Native.vstack ~spacing:8.
    [
      Native.text title;
      (match todos with
      | [] -> Native.text "Nothing here right now."
      | todos ->
          Native.list todos
            ~key:(fun todo -> string_of_int todo.id)
            ~row:(todo_row graph ~set_todos all_todos));
    ]

let component graph =
  let draft, set_draft = Native.Graph.state graph ~key:"draft" "" in
  let todos, set_todos = Native.Graph.state graph ~key:"todos" [] in
  let next_id, set_next_id = Native.Graph.state graph ~key:"next-id" 1 in
  let add_todo () =
    let title = String.trim draft in
    if title <> "" then (
      set_todos ({ id = next_id; title; completed = false } :: todos) ();
      set_next_id (next_id + 1) ();
      set_draft "" ())
  in
  let active = active_todos todos in
  let completed = completed_todos todos in
  Native.vstack ~spacing:16.
    [
      Native.text "Todos";
      Native.hstack ~spacing:8.
        [
          Native.text_field ~text:draft ~placeholder:"New task"
            ~on_change:set_draft ();
          Native.button "Add" ~on_click:add_todo;
        ];
      Native.text
        (Printf.sprintf "%d active, %d completed" (List.length active)
           (List.length completed));
      Native.hstack ~spacing:24.
        [
          todo_list graph ~title:"Active" active ~set_todos todos;
          todo_list graph ~title:"Done" completed ~set_todos todos;
        ];
    ]
  |> Native.padding

let app = App.create component

let renderer =
  create_renderer "app"
    (fun () -> App.render_json app)
    (fun event_id -> App.dispatch_click app event_id)
    (fun event_id text -> App.dispatch_change app event_id ~text)

let () = render renderer
