module Todos = Todo_core
module Apple = Bonsai_apple

type controls = {
  route : Todos.Screen.Route.t;
  search : string;
  selected_todo_id : string;
  mobile_tab : string;
  mobile_new_task_presented : bool;
  editing_todo_id : string;
  visible_todo_limit : int;
  set_route : Todos.Screen.Route.t -> unit Apple.Action.t;
  set_search : string -> unit Apple.Action.t;
  set_selected_todo_id : string -> unit Apple.Action.t;
  set_mobile_tab : string -> unit Apple.Action.t;
  set_mobile_new_task_presented : bool -> unit Apple.Action.t;
  set_editing_todo_id : string -> unit Apple.Action.t;
  set_visible_todo_limit : int -> unit Apple.Action.t;
}

val default_controls : controls
val view : ?controls:controls -> Todos.Controller.t -> Bonsai_apple.node
val mobile_view : ?controls:controls -> Todos.Controller.t -> Bonsai_apple.node

val adaptive_view :
  ?controls:controls -> Todos.Controller.t -> Bonsai_apple.node

val component :
  ?run_command:
    (dispatch:(Todos.Action.t -> unit Apple.Action.t) ->
    Todos.Command.t ->
    unit Apple.Action.t) ->
  Apple.graph ->
  Bonsai_apple.node

val adaptive_component :
  ?run_command:
    (dispatch:(Todos.Action.t -> unit Apple.Action.t) ->
    Todos.Command.t ->
    unit Apple.Action.t) ->
  Apple.graph ->
  Bonsai_apple.node
