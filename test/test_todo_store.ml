open Todos

let failf fmt = Printf.ksprintf failwith fmt

let assert_equal_int label expected actual =
  if expected <> actual then failf "%s: expected %d, got %d" label expected actual

let assert_equal_titles label expected actual =
  let titles = List.map (fun todo -> todo.Todo_store.title) actual in
  if expected <> titles then
    failf "%s: expected [%s], got [%s]" label (String.concat "; " expected) (String.concat "; " titles)

let assert_equal_strings label expected actual =
  if expected <> actual then
    failf "%s: expected [%s], got [%s]" label (String.concat "; " expected) (String.concat "; " actual)

let assert_equal_bool label expected actual =
  if expected <> actual then failf "%s: expected %b, got %b" label expected actual

let assert_equal_string label expected actual =
  if expected <> actual then failf "%s: expected %s, got %s" label expected actual

let datascript_string_result = function
  | Datascript.Result_value (Datascript.String value) -> value
  | _ -> failwith "expected DataScript string result"

let with_temp_db f =
  let dir =
    Filename.concat
      (Filename.get_temp_dir_name ())
      ("todos_ocaml_sqlite_" ^ string_of_int (Random.bits ()))
  in
  Unix.mkdir dir 0o755;
  let db_path = Filename.concat dir "todos.sqlite3" in
  Fun.protect
    ~finally:(fun () ->
      if Sys.file_exists db_path then Sys.remove db_path;
      if Sys.file_exists dir then Unix.rmdir dir)
    (fun () -> f db_path)

let test_add_lists_newest_first () =
  let store =
    Todo_store.empty ()
    |> Todo_store.add ~title:"Plan week"
    |> Todo_store.add ~title:"Buy milk"
  in
  assert_equal_titles "all todos" [ "Buy milk"; "Plan week" ] (Todo_store.all store);
  assert_equal_int "active count" 2 (Todo_store.active_count store);
  assert_equal_int "completed count" 0 (Todo_store.completed_count store)

let test_blank_titles_are_ignored () =
  let store =
    Todo_store.empty ()
    |> Todo_store.add ~title:"  "
    |> Todo_store.add ~title:"Write notes"
  in
  assert_equal_titles "non-blank todos" [ "Write notes" ] (Todo_store.all store)

let test_toggle_updates_completion () =
  let store = Todo_store.empty () |> Todo_store.add ~title:"Ship app" in
  let id = (List.hd (Todo_store.all store)).id in
  let toggled = Todo_store.toggle store ~id in
  match Todo_store.all toggled with
  | [ todo ] ->
    assert_equal_bool "todo completed" true todo.completed;
    assert_equal_int "active count" 0 (Todo_store.active_count toggled);
    assert_equal_int "completed count" 1 (Todo_store.completed_count toggled)
  | _ -> failwith "expected one todo"

let test_delete_removes_todo () =
  let store =
    Todo_store.empty ()
    |> Todo_store.add ~title:"Keep"
    |> Todo_store.add ~title:"Remove"
  in
  let remove_id = (List.hd (Todo_store.all store)).id in
  let store = Todo_store.delete store ~id:remove_id in
  assert_equal_titles "remaining todos" [ "Keep" ] (Todo_store.all store)

let test_rename_updates_title () =
  let store = Todo_store.empty () |> Todo_store.add ~title:"Draft task" in
  let id = (List.hd (Todo_store.all store)).id in
  let renamed = Todo_store.rename store ~id ~title:"Review native layout" in
  assert_equal_titles "renamed todo" [ "Review native layout" ] (Todo_store.all renamed);
  assert_equal_titles
    "blank rename is ignored"
    [ "Review native layout" ]
    (Todo_store.rename renamed ~id ~title:"   " |> Todo_store.all)

let test_add_and_edit_date_time () =
  let store =
    Todo_store.empty ()
    |> Todo_store.add ~title:"Plan launch" ~date:"Jun 20" ~time:"9:30 AM"
  in
  let todo = List.hd (Todo_store.all store) in
  assert_equal_string "initial date" "Jun 20" todo.date;
  assert_equal_string "initial time" "9:30 AM" todo.time;
  let edited =
    Todo_store.rename
      store
      ~id:todo.id
      ~title:"Plan launch review"
      ~date:"Jun 21"
      ~time:"2:00 PM"
  in
  match Todo_store.all edited with
  | [ todo ] ->
    assert_equal_string "edited title" "Plan launch review" todo.title;
    assert_equal_string "edited date" "Jun 21" todo.date;
    assert_equal_string "edited time" "2:00 PM" todo.time
  | _ -> failwith "expected one todo"

let test_search_is_case_insensitive_and_trims_query () =
  let store =
    Todo_store.empty ()
    |> Todo_store.add ~title:"Design Liquid Search"
    |> Todo_store.add ~title:"Buy milk"
    |> Todo_store.add ~title:"liquid glass polish"
  in
  assert_equal_titles
    "liquid search"
    [ "liquid glass polish"; "Design Liquid Search" ]
    (Todo_store.search store ~query:"  LIQUID  ");
  assert_equal_titles
    "empty search"
    [ "liquid glass polish"; "Buy milk"; "Design Liquid Search" ]
    (Todo_store.search store ~query:"")

let test_demo_store_matches_dashboard_shape () =
  let store = Todo_store.demo () in
  assert_equal_int "demo total count" 10 (List.length (Todo_store.all store));
  assert_equal_int "demo active count" 7 (Todo_store.active_count store);
  assert_equal_int "demo completed count" 3 (Todo_store.completed_count store)

let test_sqlite_store_persists_mutations_across_reload () =
  with_temp_db (fun path ->
    let store =
      Todo_store.sqlite ~seed_if_empty:false ~path ()
      |> Todo_store.add ~title:"Persist me" ~date:"Jun 19" ~time:"8:15 AM"
      |> Todo_store.add ~title:"Delete me"
    in
    let todos = Todo_store.all store in
    let delete_id = (List.find (fun todo -> todo.Todo_store.title = "Delete me") todos).id in
    let persist_id = (List.find (fun todo -> todo.Todo_store.title = "Persist me") todos).id in
    let store =
      store
      |> Todo_store.delete ~id:delete_id
      |> Todo_store.toggle ~id:persist_id
      |> Todo_store.rename
           ~id:persist_id
           ~title:"Still persisted"
           ~date:"Jun 20"
           ~time:"9:45 AM"
    in
    ignore store;
    match Todo_store.sqlite ~seed_if_empty:false ~path () |> Todo_store.all with
    | [ todo ] ->
      assert_equal_string "persisted title" "Still persisted" todo.title;
      assert_equal_bool "persisted completion" true todo.completed;
      assert_equal_string "persisted date" "Jun 20" todo.date;
      assert_equal_string "persisted time" "9:45 AM" todo.time
    | todos ->
      failf "persisted todos: expected 1, got %d" (List.length todos))

let test_sqlite_store_uses_datascript_storage () =
  with_temp_db (fun path ->
    let store =
      Todo_store.sqlite ~seed_if_empty:false ~path ()
      |> Todo_store.add ~title:"Stored by DataScript" ~date:"Today" ~time:"5:30 PM"
    in
    ignore store;
    let storage = Todo_sqlite.storage path in
    assert_equal_strings
      "datascript storage addresses"
      [ "datascript/root"; "datascript/tail" ]
      (Datascript.storage_addresses storage);
    match Datascript.restore storage with
    | None -> failwith "DataScript storage should restore a db"
    | Some db ->
      let titles =
        Datascript.q_string
          db
          "[:find ?title
            :where [?e :todo/title ?title]]"
        |> List.concat_map (function
          | [ title ] -> [ datascript_string_result title ]
          | _ -> failwith "unexpected title query row")
      in
      assert_equal_strings
        "datascript restored todos"
        [ "Stored by DataScript" ]
        titles)

let test_sqlite_store_seeds_demo_only_once () =
  with_temp_db (fun path ->
    let seeded = Todo_store.sqlite ~path () in
    assert_equal_int "seeded demo count" 10 (List.length (Todo_store.all seeded));
    let first_id = (List.hd (Todo_store.all seeded)).id in
    let edited =
      seeded
      |> Todo_store.delete ~id:first_id
      |> Todo_store.add ~title:"My own task" ~date:"Today" ~time:"4:00 PM"
    in
    ignore edited;
    let reloaded = Todo_store.sqlite ~path () in
    assert_equal_int "does not seed again" 10 (List.length (Todo_store.all reloaded));
    assert_equal_titles
      "keeps user task after reload"
      [ "My own task"
      ; "Grocery shopping"
      ; "Workout"
      ; "Marketing sync"
      ; "Update documentation"
      ; "User research review"
      ; "Prepare presentation"
      ; "Team stand-up meeting"
      ; "Reply to client email"
      ; "Design onboarding flow"
      ]
      (Todo_store.all reloaded))

let () =
  test_add_lists_newest_first ();
  test_blank_titles_are_ignored ();
  test_toggle_updates_completion ();
  test_delete_removes_todo ();
  test_rename_updates_title ();
  test_add_and_edit_date_time ();
  test_search_is_case_insensitive_and_trims_query ();
  test_demo_store_matches_dashboard_shape ();
  test_sqlite_store_persists_mutations_across_reload ();
  test_sqlite_store_uses_datascript_storage ();
  test_sqlite_store_seeds_demo_only_once ()
