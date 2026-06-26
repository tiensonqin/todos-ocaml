module Option = struct
  include Stdlib.Option

  let value option ~default =
    match option with Some value -> value | None -> default

  let value_exn = function
    | Some value -> value
    | None -> invalid_arg "expected Some"

  let is_some = function Some _ -> true | None -> false

  let map option ~f =
    match option with Some value -> Some (f value) | None -> None

  let iter option ~f = match option with Some value -> f value | None -> ()
  let bind option ~f = match option with Some value -> f value | None -> None
end

module List = struct
  include Stdlib.List

  let map values ~f = Stdlib.List.map f values
  let filter values ~f = Stdlib.List.filter f values
  let filter_map values ~f = Stdlib.List.filter_map f values
  let find_map values ~f = Stdlib.List.find_map f values
  let fold values ~init ~f = Stdlib.List.fold_left f init values
  let iter values ~f = Stdlib.List.iter f values
  let exists values ~f = Stdlib.List.exists f values
  let sort values ~compare = Stdlib.List.sort compare values

  let max_elt values ~compare =
    match values with
    | [] -> None
    | first :: rest ->
        Some
          (Stdlib.List.fold_left
             (fun best value -> if compare value best > 0 then value else best)
             first rest)

  let find values ~f = Stdlib.List.find_opt f values
  let nth_exn values index = Stdlib.List.nth values index
  let iter2_exn left right ~f = Stdlib.List.iter2 f left right
end

module String = struct
  include Stdlib.String

  let strip = Stdlib.String.trim
  let lowercase = Stdlib.String.lowercase_ascii
  let is_empty value = Stdlib.String.length value = 0

  let is_prefix value ~prefix =
    let value_length = Stdlib.String.length value in
    let prefix_length = Stdlib.String.length prefix in
    value_length >= prefix_length
    && Stdlib.String.sub value 0 prefix_length = prefix

  let is_substring value ~substring =
    let value_length = Stdlib.String.length value in
    let substring_length = Stdlib.String.length substring in
    let rec loop index =
      index + substring_length <= value_length
      && (Stdlib.String.sub value index substring_length = substring
         || loop (index + 1))
    in
    substring_length = 0 || loop 0

  let split_lines value =
    let rec loop start index acc =
      if index = Stdlib.String.length value then
        Stdlib.List.rev (Stdlib.String.sub value start (index - start) :: acc)
      else if value.[index] = '\n' then
        loop (index + 1) (index + 1)
          (Stdlib.String.sub value start (index - start) :: acc)
      else loop start (index + 1) acc
    in
    if Stdlib.String.length value = 0 then [] else loop 0 0 []

  module Table = struct
    type 'a t = (string, 'a) Stdlib.Hashtbl.t

    let create () = Stdlib.Hashtbl.create 16
  end
end

module Hashtbl = struct
  type ('key, 'value) t = ('key, 'value) Stdlib.Hashtbl.t

  let create _ = Stdlib.Hashtbl.create 16
  let find table key = Stdlib.Hashtbl.find_opt table key
  let find_exn table key = Stdlib.Hashtbl.find table key
  let set table ~key ~data = Stdlib.Hashtbl.replace table key data
  let remove = Stdlib.Hashtbl.remove
  let keys table = Stdlib.Hashtbl.fold (fun key _ acc -> key :: acc) table []

  let data table =
    Stdlib.Hashtbl.fold (fun _ value acc -> value :: acc) table []
end

module Result = struct
  include Stdlib.Result

  let try_with f = try Ok (f ()) with exn -> Error exn
end

module Exn = struct
  let to_string = Printexc.to_string
end

module Int = struct
  include Stdlib.Int

  let min_value = Stdlib.min_int
  let max_value = Stdlib.max_int
  let to_string = Stdlib.string_of_int
end

module Int64 = struct
  include Stdlib.Int64

  let to_int_exn = Stdlib.Int64.to_int
end

module Bool = Stdlib.Bool

let failwithf format = Printf.ksprintf failwith format
