open Crowbar

exception Encode_error of string
exception Decode_error

(** Pretty printers *)

let register_printer () =
  Printexc.register_printer (function
    | Encode_error err -> Some (Fmt.strf "(Encoding error: %s)" err)
    | Decode_error -> Some (Fmt.strf "(Decoding error: decoding was not Ok)")
    | _ -> None)

let pp_chr =
  let escaped = function
    | ' ' .. '~' as c -> String.make 1 c
    | _ -> "."
  in
    Fmt.using escaped Fmt.string

let pp_scalar : type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l then
        Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if
      (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp = pp_scalar ~get:String.get ~length:String.length

(** Encoding and decoding *)

let check_encode str =
  let subs = Astring.String.cuts ~sep:"\r\n" str in
  let check str =
    if String.length str > 78 then
      raise (Encode_error "too long string returned")
  in
  List.iter check subs; str

let encode input =
  let buf = Buffer.create 80 in
  let encoder = Rfc2045.encoder (`Buffer buf) in
  String.iter
    (fun c ->
      let ret = Rfc2045.encode encoder (`Char c) in
      match ret with `Ok -> () | _ -> assert false ) 
    input;
  let encode = Rfc2045.encode encoder (`End) in
  match encode with
  | `Ok -> Buffer.contents buf |> check_encode
  | _ -> assert false

let decode input =
  let decoder = Rfc2045.decoder (`String input) in
  let rec rec_decode acc =
    match Rfc2045.decode decoder with
    | `End -> acc
    | `Flush output ->
        rec_decode (Bytes.(of_string output |> copy |> to_string)::acc)
    | `Malformed _ -> raise Decode_error
    | _ -> assert false
  in
  List.fold_left ( ^ ) "" (List.rev (rec_decode []))

(** String generators *)

let bytes_fixed_range : string gen = dynamic_bind (range 78) bytes_fixed

let char_from_alpha alpha : string gen = map [range (String.length alpha)] (fun i -> String.get alpha i |> String.make 1)
let string_from_alpha n =
  let acc = const "" in
  let alpha = Rfc2045.default_alphabet in
  let rec add_char_from_alpha alpha acc = function
  | 0 -> acc
  | n -> add_char_from_alpha alpha (concat_gen_list (const "") [acc; char_from_alpha alpha]) (n - 1) in
  add_char_from_alpha alpha acc n
let random_string_from_alpha n = dynamic_bind (range n) string_from_alpha

let bytes_fixed_range_from_alpha : string gen = dynamic_bind (range 78) bytes_fixed

let add_padding str =
  let str = (str ^ "A==") in
  String.sub str 0 (String.length str / 4 * 4) 

(** Tests *)

let e2d inputs =
  let input = List.fold_left (fun acc s -> acc ^ "\r\n" ^ s) "" inputs in
  let encode = encode input in
  let decode = decode encode in
  check_eq ~pp ~cmp:String.compare ~eq:String.equal input decode

let d2e inputs end_input =
  let end_input = add_padding end_input in
  let inputs = inputs @ [end_input] in
  let input = List.fold_left (fun acc s -> if String.length s <> 0 then acc ^ "\r\n" ^ s else acc) (List.hd inputs) (List.tl inputs) in
  let decode = decode input in
  let encode = encode decode in
  check_eq ~pp ~cmp:String.compare ~eq:String.equal input encode

let () =
  register_printer ();
  add_test ~name:("rfc2045: encode -> decode") [list (bytes_fixed_range)] e2d;
  add_test ~name:("rfc2045: decode -> encode") [list (string_from_alpha 76); random_string_from_alpha 76] d2e
