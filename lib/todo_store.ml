type todo =
  { id : int
  ; title : string
  ; completed : bool
  ; date : string
  ; time : string
  }

type todo_row =
  { todo : todo
  ; created_at : int
  }

type t =
  { db : Datascript.db
  ; next_created_at : int
  }

let title_attr = "todo/title"
let completed_attr = "todo/completed"
let created_at_attr = "todo/created-at"
let date_attr = "todo/date"
let time_attr = "todo/time"

let empty () = { db = Datascript.empty_db (); next_created_at = 0 }

let string_result = function
  | Datascript.Result_value (Datascript.String value) -> value
  | _ -> invalid_arg "expected string query result"
;;

let bool_result = function
  | Datascript.Result_value (Datascript.Bool value) -> value
  | _ -> invalid_arg "expected bool query result"
;;

let int_result = function
  | Datascript.Result_value (Datascript.Int value) -> value
  | _ -> invalid_arg "expected int query result"
;;

let todo_of_row = function
  | [ Datascript.Result_entity id; title; completed; created_at; date; time ] ->
    { todo =
        { id
        ; title = string_result title
        ; completed = bool_result completed
        ; date = string_result date
        ; time = string_result time
        }
    ; created_at = int_result created_at
    }
  | _ -> invalid_arg "unexpected todo query row"
;;

let rows db =
  Datascript.q_string
    db
    "[:find ?e ?title ?completed ?created-at ?date ?time
      :where
      [?e :todo/title ?title]
      [?e :todo/completed ?completed]
      [?e :todo/created-at ?created-at]
      [?e :todo/date ?date]
      [?e :todo/time ?time]]"
;;

let all t =
  rows t.db
  |> List.map todo_of_row
  |> List.sort (fun left right -> compare right.created_at left.created_at)
  |> List.map (fun row -> row.todo)
;;

let add ?(date = "") ?(time = "") t ~title =
  let title = String.trim title in
  if title = ""
  then t
  else (
    let created_at = t.next_created_at + 1 in
    let report =
      Datascript.transact
        t.db
        [ Datascript.Add (Datascript.Temp_id "todo", title_attr, Datascript.String title)
        ; Datascript.Add (Datascript.Temp_id "todo", completed_attr, Datascript.Bool false)
        ; Datascript.Add (Datascript.Temp_id "todo", created_at_attr, Datascript.Int created_at)
        ; Datascript.Add (Datascript.Temp_id "todo", date_attr, Datascript.String (String.trim date))
        ; Datascript.Add (Datascript.Temp_id "todo", time_attr, Datascript.String (String.trim time))
        ]
    in
    { db = report.Datascript.db_after; next_created_at = created_at })
;;

let find_todo t ~id =
  all t |> List.find_opt (fun todo -> todo.id = id)
;;

let toggle t ~id =
  match find_todo t ~id with
  | None -> t
  | Some todo ->
    let report =
      Datascript.transact
        t.db
        [ Datascript.Add
            (Datascript.Entity_id id, completed_attr, Datascript.Bool (not todo.completed))
        ]
    in
    { t with db = report.Datascript.db_after }
;;

let delete t ~id =
  match find_todo t ~id with
  | None -> t
  | Some _ ->
    let report = Datascript.transact t.db [ Datascript.RetractEntity (Datascript.Entity_id id) ] in
    { t with db = report.Datascript.db_after }
;;

let rename ?date ?time t ~id ~title =
  let title = String.trim title in
  if title = ""
  then t
  else (
    match find_todo t ~id with
    | None -> t
    | Some _ ->
      let report =
        Datascript.transact
          t.db
          ([ Datascript.Add (Datascript.Entity_id id, title_attr, Datascript.String title) ]
           @ (match date with
              | None -> []
              | Some date ->
                [ Datascript.Add
                    (Datascript.Entity_id id, date_attr, Datascript.String (String.trim date))
                ])
           @ (match time with
              | None -> []
              | Some time ->
                [ Datascript.Add
                    (Datascript.Entity_id id, time_attr, Datascript.String (String.trim time))
                ]))
      in
      { t with db = report.Datascript.db_after })
;;

let demo () =
  let tasks =
    [ "Design onboarding flow", false, "Today", "10:00 AM"
    ; "Reply to client email", false, "Today", "12:30 PM"
    ; "Team stand-up meeting", false, "Today", "3:00 PM"
    ; "Prepare presentation", false, "Tomorrow", ""
    ; "User research review", false, "May 28", ""
    ; "Update documentation", false, "May 29", ""
    ; "Marketing sync", false, "May 30", ""
    ; "Workout", true, "Today", "7:00 AM"
    ; "Grocery shopping", true, "Yesterday", ""
    ; "Review new designs", true, "May 27", ""
    ]
  in
  List.fold_left
    (fun store (title, completed, date, time) ->
       let store = add store ~title ~date ~time in
       if completed
       then (
         match all store with
         | todo :: _ -> toggle store ~id:todo.id
         | [] -> store)
       else store)
    (empty ())
    tasks
;;

let normalized value = value |> String.trim |> String.lowercase_ascii

let contains ~substring value =
  let value_length = String.length value in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > value_length
    then false
    else if String.sub value index substring_length = substring
    then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0
;;

let search t ~query =
  match normalized query with
  | "" -> all t
  | query ->
    all t
    |> List.filter (fun todo ->
      contains (normalized todo.title) ~substring:query)
;;

let count_by t ~f = all t |> List.filter f |> List.length
let active_count t = count_by t ~f:(fun todo -> not todo.completed)
let completed_count t = count_by t ~f:(fun todo -> todo.completed)
