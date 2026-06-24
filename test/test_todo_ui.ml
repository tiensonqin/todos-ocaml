open Core

module Apple = Bonsai_apple
module Backend = Apple.For_testing.Backend
module Renderer = Apple.Renderer.Make (Backend)
module Todos = Todo_core

let failf fmt = Printf.ksprintf failwith fmt

let assert_contains label text ~substring =
  if not (String.is_substring text ~substring)
  then failf "%s: expected output to contain %S, got:\n%s" label substring text
;;

let assert_not_contains label text ~substring =
  if String.is_substring text ~substring
  then failf "%s: expected output not to contain %S, got:\n%s" label substring text
;;

let assert_no_empty_label label text =
  let has_empty_label =
    String.split_lines text
    |> List.exists ~f:(fun line ->
      String.is_substring line ~substring:"label#"
      && String.is_substring line ~substring:"text=\"\"")
  in
  if has_empty_label then failf "%s: expected output not to contain an empty label, got:\n%s" label text
;;

let todo ?(completed = false) ~id ~title ~created_at_ms () : Todos.Todo.t =
  { id; title; completed; created_at_ms }
;;

let render ?(controls = Todo_ui.default_controls) model =
  Backend.reset ();
  let controller : Todos.Controller.t =
    { model; dispatch = (fun _action -> Bonsai.Effect.Ignore) }
  in
  let mounted =
    Renderer.mount
      ~schedule_event:(fun _ -> ())
      (Todo_ui.view controller ~controls)
  in
  Backend.show (Renderer.view mounted)
;;

let render_mobile ?(controls = Todo_ui.default_controls) model =
  Backend.reset ();
  let controller : Todos.Controller.t =
    { model; dispatch = (fun _action -> Bonsai.Effect.Ignore) }
  in
  let mounted =
    Renderer.mount
      ~schedule_event:(fun _ -> ())
      (Todo_ui.mobile_view controller ~controls)
  in
  Backend.show (Renderer.view mounted)
;;

let render_adaptive ?(controls = Todo_ui.default_controls) model =
  Backend.reset ();
  let controller : Todos.Controller.t =
    { model; dispatch = (fun _action -> Bonsai.Effect.Ignore) }
  in
  let mounted =
    Renderer.mount
      ~schedule_event:(fun _ -> ())
      (Todo_ui.adaptive_view controller ~controls)
  in
  Backend.show (Renderer.view mounted)
;;

let test_empty_model_renders_split_view_composer_and_search () =
  let output = render Todos.Model.initial in
  assert_contains "all route" output ~substring:"Tasks";
  assert_contains "active route" output ~substring:"Active";
  assert_contains "completed route" output ~substring:"Completed";
  assert_contains "searchable" output ~substring:"searchable";
  assert_contains "composer" output ~substring:"placeholder=\"New task\"";
  assert_contains "split root" output ~substring:"navigation-split"
;;

let test_loaded_model_renders_split_view_and_tasks () =
  let model =
    { Todos.Model.initial with
      todos =
        [ todo ~id:"todo-1" ~title:"iOS UI" ~created_at_ms:10 ()
        ; todo ~id:"todo-2" ~title:"Mac desktop" ~created_at_ms:20 ~completed:true ()
        ]
    }
  in
  let output = render model in
  assert_contains "split title" output ~substring:"Tasks";
  assert_contains "active task" output ~substring:"iOS UI";
  assert_contains "completed task" output ~substring:"Mac desktop";
  assert_contains "counts" output ~substring:"1 active, 1 completed"
;;

let test_mobile_model_renders_tabbed_phone_ui () =
  let model =
    { Todos.Model.initial with
      todos =
        [ todo ~id:"todo-1" ~title:"iOS UI" ~created_at_ms:10 ()
        ; todo ~id:"todo-2" ~title:"Done task" ~created_at_ms:20 ~completed:true ()
        ]
    }
  in
  let output = render_mobile model in
  assert_contains "mobile tabs" output ~substring:"tab-view";
  assert_contains "tasks tab" output ~substring:"all:Tasks";
  assert_contains "active tab" output ~substring:"active:Active";
  assert_contains "completed tab" output ~substring:"completed:Completed";
  assert_contains "shared row" output ~substring:"iOS UI";
  assert_contains "mobile search" output ~substring:"searchable";
  assert_not_contains "mobile fixed input width" output ~substring:"modifiers=[frame]";
  assert_no_empty_label "mobile empty error gap" output;
  if String.is_substring output ~substring:"navigation-split"
  then failf "mobile output should not render navigation-split, got:\n%s" output
;;

let test_adaptive_model_contains_phone_and_regular_layouts () =
  let output = render_adaptive Todos.Model.initial in
  assert_contains "adaptive root" output ~substring:"adaptive-layout";
  assert_contains "phone tabs" output ~substring:"tab-view";
  assert_contains "phone search" output ~substring:"searchable";
  assert_contains "regular split" output ~substring:"navigation-split"
;;

let test_search_filters_visible_tasks () =
  let model =
    { Todos.Model.initial with
      todos =
        [ todo ~id:"todo-1" ~title:"iOS UI" ~created_at_ms:10 ()
        ; todo ~id:"todo-2" ~title:"Web worker" ~created_at_ms:20 ()
        ]
    }
  in
  let controls = { Todo_ui.default_controls with search = "web" } in
  let output = render model ~controls in
  assert_contains "matching task" output ~substring:"Web worker";
  if String.is_substring output ~substring:"iOS UI"
  then failf "filtered output should not contain iOS UI, got:\n%s" output
;;

let () =
  test_empty_model_renders_split_view_composer_and_search ();
  test_loaded_model_renders_split_view_and_tasks ();
  test_mobile_model_renders_tabbed_phone_ui ();
  test_adaptive_model_contains_phone_and_regular_layouts ();
  test_search_filters_visible_tasks ()
;;
