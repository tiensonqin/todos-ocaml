external exec : string -> string -> unit = "todos_sqlite_exec"
external select_content : string -> int -> string option = "todos_sqlite_select_content"
external list_addresses : string -> int list = "todos_sqlite_list_addresses"

let hex_digit value =
  Char.unsafe_chr (if value < 10 then Char.code '0' + value else Char.code 'a' + value - 10)
;;

let hex_encode bytes =
  String.init
    (String.length bytes * 2)
    (fun index ->
      let byte = Char.code bytes.[index / 2] in
      if index mod 2 = 0 then hex_digit (byte lsr 4) else hex_digit (byte land 0x0f))
;;

let hex_value = function
  | '0' .. '9' as ch -> Char.code ch - Char.code '0'
  | 'a' .. 'f' as ch -> Char.code ch - Char.code 'a' + 10
  | 'A' .. 'F' as ch -> Char.code ch - Char.code 'A' + 10
  | ch -> invalid_arg ("invalid hex digit: " ^ String.make 1 ch)
;;

let hex_decode encoded =
  if String.length encoded mod 2 <> 0 then invalid_arg "hex string has odd length";
  String.init
    (String.length encoded / 2)
    (fun index ->
      let high = hex_value encoded.[index * 2] in
      let low = hex_value encoded.[index * 2 + 1] in
      Char.chr ((high lsl 4) lor low))
;;

let starts_with prefix value =
  let prefix_len = String.length prefix in
  String.length value >= prefix_len && String.sub value 0 prefix_len = prefix
;;

let ocaml_payload_prefix = "ocaml-marshal:"

let sql_quote text =
  "'" ^ String.concat "''" (String.split_on_char '\'' text) ^ "'"
;;

let payload_to_content payload =
  ocaml_payload_prefix ^ hex_encode (Marshal.to_string payload [])
;;

let payload_of_content content =
  if starts_with ocaml_payload_prefix content
  then (
    let encoded =
      String.sub
        content
        (String.length ocaml_payload_prefix)
        (String.length content - String.length ocaml_payload_prefix)
    in
    Some (Marshal.from_string (hex_decode encoded) 0 : Datascript.storage_payload))
  else None
;;

let sqlite_addr_of_storage_address = function
  | "datascript/root" | "0" -> 0
  | "datascript/tail" | "1" -> 1
  | address -> int_of_string address
;;

let storage_address_of_sqlite_addr = function
  | 0 -> "datascript/root"
  | 1 -> "datascript/tail"
  | address -> string_of_int address
;;

let ensure_schema path =
  exec
    path
    "create table if not exists kvs (
       addr integer primary key,
       content text,
       addresses text
     );"
;;

let upsert_sql (address, payload) =
  Printf.sprintf
    "insert into kvs (addr, content, addresses) values (%d, %s, null)
     on conflict(addr) do update set content = excluded.content, addresses = excluded.addresses;"
    (sqlite_addr_of_storage_address address)
    (sql_quote (payload_to_content payload))
;;

let delete_sql addresses =
  match addresses with
  | [] -> ""
  | _ ->
    "delete from kvs where addr in ("
    ^ (addresses
       |> List.map sqlite_addr_of_storage_address
       |> List.map string_of_int
       |> String.concat ",")
    ^ ");"
;;

let storage path =
  ensure_schema path;
  let storage_store entries =
    let sql =
      entries |> List.map upsert_sql |> List.filter (fun sql -> sql <> "") |> String.concat "\n"
    in
    if sql <> "" then exec path sql
  in
  let storage_restore address =
    match select_content path (sqlite_addr_of_storage_address address) with
    | None -> None
    | Some content -> payload_of_content content
  in
  let storage_list_addresses () =
    ensure_schema path;
    list_addresses path |> List.map storage_address_of_sqlite_addr
  in
  let storage_delete addresses =
    match delete_sql addresses with
    | "" -> ()
    | sql -> exec path sql
  in
  { Datascript.storage_store; storage_restore; storage_list_addresses; storage_delete }
;;
