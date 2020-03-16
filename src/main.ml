let domain = "test.net"
let port = 8001
let password = "hello"

type command =
  | Init of { password : [ `Clear of string ] }
  | Info of string

let escape_commas (s: string) =
  let b = Buffer.create 20 in
  String.iter (function
    | '\\' -> Buffer.add_string b "\\\\"
    | ',' -> Buffer.add_string b "\\,"
    | c -> Buffer.add_char b c
  ) s;
  Buffer.contents b

let serialize_command (cmd: command): string =
  match cmd with
  | Init { password = `Clear pwd } ->
    Printf.sprintf "init password=%s,compression=off"
      (escape_commas pwd)
  | Info str ->
    Printf.sprintf "info %s" str

let read_all cin =
  let buf = Buffer.create 1024 in
  let b = Bytes.create 1024 in
  let rec loop () =
    let bytes_read = try input cin b 0 1024 with Sys_blocked_io -> 0 in
    Buffer.add_subbytes buf b 0 bytes_read;
    if bytes_read = 0 then ()
    else loop ()
  in
  loop ();
  Buffer.contents buf

module P = struct
  open Angstrom

  let any_string =
    BE.any_int32 >>= fun len ->
    assert (len <> -1l);
    take (Int32.to_int len)

  (* Objects *)

  (* an object of type ['a obj_ty] can be parsed into a value of type ['a] *)
  type _ obj_ty =
    | Char : char obj_ty
    | Int : int32 obj_ty
    | Long : string obj_ty
    | String : string option obj_ty
    | Buffer : string option obj_ty
    | Pointer : string option obj_ty
    | Time : string obj_ty
    | Hashtable : hashtable_ty obj_ty
    | Hdata : hdata_ty obj_ty
    | Info : (string * string) obj_ty
    | Infolist : infolist_ty obj_ty
    | Array : array_ty obj_ty

  and hashtable_ty =
    | HashtableTy : 'a obj_ty * 'b obj_ty * ('a * 'b) list -> hashtable_ty

  and hdata_ty =
    | HdataTy : string * 'ks hdata_keys * (string option * 'ks) list -> hdata_ty

  and infolist_ty =
    | InfolistTy : string * 'a infolist_items -> infolist_ty

  and array_ty =
    | ArrayTy : 'a obj_ty * 'a list -> array_ty

  and _ hdata_keys =
    | Hdata_keys_nil : unit hdata_keys
    | Hdata_keys_cons : string * 'a obj_ty * 'b hdata_keys -> ('a * 'b) hdata_keys

  and _ infolist_items =
    | Infolist_items_nil : unit infolist_items
    | Infolist_items_cons : 'a infolist_item * 'b infolist_items -> ('a * 'b) infolist_items

  and _ infolist_item =
    | Infolist_item_nil : unit infolist_item
    | Infolist_item_cons : string * 'a obj_ty * 'a * 'b infolist_item -> ('a * 'b) infolist_item

  let char_ty = Angstrom.string "chr" >>| fun _ -> Char
  let int_ty = Angstrom.string "int" >>| fun _ -> Int
  let long_ty = Angstrom.string "lon" >>| fun _ -> Long
  let string_ty = Angstrom.string "str" >>| fun _ -> String
  let buffer_ty = Angstrom.string "buf" >>| fun _ -> Buffer
  let pointer_ty = Angstrom.string "ptr" >>| fun _ -> Pointer
  let time_ty = Angstrom.string "tim" >>| fun _ -> Time
  let hashtable_ty = Angstrom.string "htb" >>| fun _ -> Hashtable
  let hdata_ty = Angstrom.string "hda" >>| fun _ -> Hdata
  let info_ty = Angstrom.string "inf" >>| fun _ -> Info
  let infolist_ty = Angstrom.string "inl" >>| fun _ -> Infolist
  let array_ty = Angstrom.string "arr" >>| fun _ -> Array

  type any_obj_ty =
    | AnyObjTy : 'a obj_ty -> any_obj_ty [@@ocaml.unboxed]

  type any_hdata_keys =
    | AnyHdataKeys : 'a hdata_keys -> any_hdata_keys [@@ocaml.unboxed]

  type any_infolist_item =
    | AnyInfolistItem : 'a infolist_item -> any_infolist_item [@@ocaml.unboxed]

  type any_infolist_items =
    | AnyInfolistItems : 'a infolist_items -> any_infolist_items [@@ocaml.unboxed]

  type any_obj =
    | AnyObj : 'a obj_ty * 'a -> any_obj

  let pack_obj_ty x = AnyObjTy x

  let obj_ty : any_obj_ty t =
    choice [
      char_ty >>| pack_obj_ty;
      int_ty >>| pack_obj_ty;
      long_ty >>| pack_obj_ty;
      string_ty >>| pack_obj_ty;
      buffer_ty >>| pack_obj_ty;
      pointer_ty >>| pack_obj_ty;
      time_ty >>| pack_obj_ty;
      hashtable_ty >>| pack_obj_ty;
      hdata_ty >>| pack_obj_ty;
      info_ty >>| pack_obj_ty;
      infolist_ty >>| pack_obj_ty;
      array_ty >>| pack_obj_ty;
    ]

  let any_string_or_null =
    BE.any_int32 >>= fun len ->
    if len = -1l then return None
    else any_string >>| fun s -> Some s

  let pointer =
    any_int8 >>= fun len ->
    take len >>| fun s ->
    if len = 1 && s.[0] = '0' then None
    else Some s

  (* FIXME: this accepts more than what the format specifies (eg "foo:bar,") *)
  let hdata_keys : any_hdata_keys t = fix (fun hdata_keys_rec ->
    (lift3 (fun x y z -> x,y,z)
       (take_while ((<>) ':') <* char ':')
       (obj_ty <* char ',')
       hdata_keys_rec >>| fun (key, AnyObjTy ty, AnyHdataKeys keys) ->
     AnyHdataKeys (Hdata_keys_cons (key, ty, keys)))
    <|>
    (lift2 (fun x y -> x,y)
       (take_while ((<>) ':') <* char ':')
       obj_ty >>| fun (key, AnyObjTy ty) ->
     AnyHdataKeys (Hdata_keys_cons (key, ty, Hdata_keys_nil)))
    <|>
    (return (AnyHdataKeys Hdata_keys_nil))
  )

  let rec obj_data : type a. a obj_ty -> a t = fun (ty: a obj_ty) ->
    match ty with
    | Char -> any_char
    | Int -> BE.any_int32
    | Long ->
      any_int8 >>= fun len -> take len
    | String -> any_string_or_null
    | Buffer -> any_string_or_null
    | Pointer -> pointer
    | Time -> any_string
    | Hashtable ->
      lift3 (fun x y z -> x,y,z)
        obj_ty obj_ty BE.any_int32 >>= fun (key_ty_packed, val_ty_packed, size) ->
      let AnyObjTy key_ty = key_ty_packed in
      let AnyObjTy val_ty = val_ty_packed in
      let data_parser =
        count (Int32.to_int size) (lift2 (fun k v -> k, v) (obj_data key_ty) (obj_data val_ty)) in
      data_parser >>| fun data -> HashtableTy (key_ty, val_ty, data)
    | Array ->
      lift2 (fun x y -> x,y) obj_ty BE.any_int32 >>= fun (ty_packed, size) ->
      let AnyObjTy ty = ty_packed in
      let size = Int32.to_int size in
      count size (obj_data ty) >>| fun data ->
      ArrayTy (ty, data)
    | Hdata ->
      lift3 (fun x y z -> x,y,z)
        any_string hdata_keys BE.any_int32 >>= fun (hpath, AnyHdataKeys keys, nbitems) ->
      count (Int32.to_int nbitems)
        (lift2 (fun ptr item -> ptr, item) pointer (hdata_item keys)) >>| fun items ->
      HdataTy (hpath, keys, items)
    | Info ->
      lift2 (fun x y -> x,y) any_string any_string
    | Infolist ->
      lift2 (fun x y -> x,y) any_string BE.any_int32 >>= fun (name, nbitems) ->
      infolist_items_rec (Int32.to_int nbitems) >>| fun (AnyInfolistItems items) ->
      InfolistTy (name, items)

  and hdata_item : type ks. ks hdata_keys -> ks t = fun keys ->
    match keys with
    | Hdata_keys_nil -> return ()
    | Hdata_keys_cons (_, ty, keys') ->
      lift2 (fun item items -> item, items)
        (obj_data ty) (hdata_item keys')

  and infolist_items_rec : int -> any_infolist_items t = fun n ->
    if n = 0 then return (AnyInfolistItems Infolist_items_nil)
    else
      BE.any_int32 >>= fun nbvalues ->
      infolist_item_rec (Int32.to_int nbvalues) >>= fun (AnyInfolistItem item) ->
      infolist_items_rec (n-1) >>| fun (AnyInfolistItems items) ->
      AnyInfolistItems (Infolist_items_cons (item, items))

  and infolist_item_rec : int -> any_infolist_item t = fun n ->
    if n = 0 then return (AnyInfolistItem Infolist_item_nil)
    else
      any_string >>= fun name ->
      obj_ty >>= fun (AnyObjTy ty) ->
      obj_data ty >>= fun v ->
      infolist_item_rec (n-1) >>| fun (AnyInfolistItem values) ->
      AnyInfolistItem (Infolist_item_cons (name, ty, v, values))

  let obj : any_obj t =
    obj_ty >>= fun (AnyObjTy ty) ->
    obj_data ty >>| fun data ->
    AnyObj (ty, data)

  let reply_header =
    BE.any_int32 <* int8 0

  let reply_data : (string * any_obj list) t =
    lift2 (fun id objects -> id, objects)
      any_string (many obj)
end

let really_input_s cin len =
  let b = Bytes.create len in
  really_input cin b 0 len;
  Bytes.unsafe_to_string b

let input_header cin =
  let s = really_input_s cin 5 in
  match Angstrom.parse_string P.reply_header s with
  | Ok i -> Int32.to_int i - 5
  | Error e -> failwith (Printf.sprintf "incorrect reply header: %s" e)

let () =
  let domain_addr = Unix.gethostbyname domain in
  assert (Array.length domain_addr.h_addr_list > 0);
  let inet = domain_addr.h_addr_list.(0) in
  let cin, cout = Unix.open_connection (Unix.ADDR_INET (inet, port)) in
  (* Unix.set_nonblock (Unix.descr_of_in_channel cin); *)
  Printf.fprintf cout "(%d) %s\n%!" 0 (serialize_command (Init { password = `Clear password }));
  print_endline "init sent";
  (* Printf.eprintf "> %s\n%!" (read_all cin); *)
  Printf.fprintf cout "(%d) %s\n%!" 1 (serialize_command (Info "version"));
  print_endline "info version";
  (* let data = read_all cin in *)
  let reply_len = input_header cin in
  Printf.eprintf "got header. message len: %d\n" reply_len;
  let data = really_input_s cin reply_len in
  Printf.eprintf "data: %s\n%!" data;
  let () =
    match Angstrom.parse_string P.reply data with
    | Ok (tag, `Info (k, v) :: _) -> Printf.eprintf "> (%s) info: %s: %s\n%!" tag k v
    | Ok (tag, `Other s :: _ ) -> Printf.eprintf "> (%s) other: %s\n%!" tag s
    | Ok (_, []) -> Printf.eprintf "err: no objects"
    | Error e -> Printf.eprintf "err: %s" e
  in
  Unix.shutdown_connection cin;
  close_in cin;
  print_endline "OK"
