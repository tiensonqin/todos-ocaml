type todo =
  { id : int
  ; title : string
  ; completed : bool
  ; date : string
  ; time : string
  }

type t

val empty : unit -> t
val demo : unit -> t
val sqlite : ?seed_if_empty:bool -> path:string -> unit -> t
val default_sqlite_path : unit -> string
val add : ?date:string -> ?time:string -> t -> title:string -> t
val toggle : t -> id:int -> t
val delete : t -> id:int -> t
val rename : ?date:string -> ?time:string -> t -> id:int -> title:string -> t
val all : t -> todo list
val search : t -> query:string -> todo list
val active_count : t -> int
val completed_count : t -> int
