module Crypto_version = struct
  open Cstruct
  
  let (//) x y =
    if y < 1 then raise Division_by_zero else
      if x > 0 then 1 + ((x - 1) / y) else 0 [@@inline]
  
  let sym     = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  let padding = int_of_char '='
  
  let (emap, dmap) =
    let make_ht f =
      let ht = Hashtbl.create 64 in
      for i = 0 to String.length sym - 1 do
        f ht i (int_of_char sym.[i])
      done ;
      Hashtbl.find ht in
    (make_ht Hashtbl.add),
    (make_ht (fun ht i c -> Hashtbl.add ht c i))
  
  let encode cs =
  
    let n   = len cs in
    let n'  = n // 3 * 4 in
    let cs' = create n' in
  
    let emit b1 b2 b3 i =
      BE.set_uint16 cs' i
        ((emap (b1 lsr 2 land 0x3f) lsl 8) lor
        (emap ((b1 lsl 4) lor (b2 lsr 4) land 0x3f))) ;
      BE.set_uint16 cs' (i + 2)
        ((emap ((b2 lsl 2) lor (b3 lsr 6) land 0x3f) lsl 8) lor
        (emap (b3 land 0x3f))) in
  
    let rec enc j = function
      | i when i = n     -> ()
      | i when i = n - 1 ->
          emit (get_uint8 cs i) 0 0 j
      | i when i = n - 2 ->
          emit (get_uint8 cs i) (get_uint8 cs (i + 1)) 0 j
      | i ->
          emit (get_uint8 cs i) (get_uint8 cs (i + 1)) (get_uint8 cs (i + 2)) j ;
          enc (j + 4) (i + 3)
  
    and fix = function
      | 0 -> ()
      | i -> set_uint8 cs' (n' - i) padding ; fix (i - 1) in
  
    enc 0 0 ;
    fix ((3 - n mod 3) mod 3) ;
    cs'
  
  let decode cs =
  
    let n   = len cs in
    let n'  = (n / 4) * 3 in
    let cs' = create n' in
  
    let emit a b c d i =
      let x = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
      BE.set_uint16 cs' i (x lsr 8) ;
      set_uint8 cs' (i + 2) (x land 0xff) in
  
    let rec dec j = function
      | i when i = n -> 0
      | i ->
          let a = dmap (get_uint8 cs i)
          and b = dmap (get_uint8 cs (i + 1))
          and (d, pad) =
            let x = get_uint8 cs (i + 3) in try (dmap x, 0) with
              | Not_found when x = padding -> (0, 1) in
          let (c, pad) =
            let x = get_uint8 cs (i + 2) in try (dmap x, pad) with
              | Not_found when x = padding && pad = 1 -> (0, 2) in
  
          emit a b c d j ;
          match pad with
          | 0 -> dec (j + 3) (i + 4)
          | _ when i + 4 <> n -> raise Not_found
          | _ -> pad
    in
    try let pad = dec 0 0 in Some (sub cs' 0 (n' - pad))
    with Invalid_argument _ | Not_found -> None
  
  
  let is_base64_char c =
    try ( ignore @@ dmap (int_of_char c) ; true )
    with Not_found -> false
end

module Old_version = struct
  let default_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

  let uri_safe_alphabet =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

  let padding = '='

  let of_char ?(alphabet = default_alphabet) x =
    if x = padding then 0 else String.index alphabet x

  let to_char ?(alphabet = default_alphabet) x = alphabet.[x]

  let decode ?alphabet input =
    let length = String.length input in
    let input =
      if length mod 4 = 0 then input
      else input ^ String.make (4 - (length mod 4)) padding
    in
    let length = String.length input in
    let words = length / 4 in
    let padding =
      match length with
      | 0 -> 0
      | _ when input.[length - 2] = padding -> 2
      | _ when input.[length - 1] = padding -> 1
      | _ -> 0
    in
    let output = Bytes.make ((words * 3) - padding) '\000' in
    for i = 0 to words - 1 do
      let a = of_char ?alphabet input.[(4 * i) + 0]
      and b = of_char ?alphabet input.[(4 * i) + 1]
      and c = of_char ?alphabet input.[(4 * i) + 2]
      and d = of_char ?alphabet input.[(4 * i) + 3] in
      let n = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
      let x = (n lsr 16) land 255
      and y = (n lsr 8) land 255
      and z = n land 255 in
      Bytes.set output ((3 * i) + 0) (char_of_int x) ;
      if i <> words - 1 || padding < 2 then
        Bytes.set output ((3 * i) + 1) (char_of_int y) ;
      if i <> words - 1 || padding < 1 then
        Bytes.set output ((3 * i) + 2) (char_of_int z)
    done ;
    Bytes.unsafe_to_string output

  let decode_opt ?alphabet input =
    try Some (decode ?alphabet input) with Not_found -> None

  let encode ?(pad = true) ?alphabet input =
    let length = String.length input in
    let words = (length + 2) / 3 (* rounded up *) in
    let padding_len = if length mod 3 = 0 then 0 else 3 - (length mod 3) in
    let output = Bytes.make (words * 4) '\000' in
    let get i = if i >= length then 0 else int_of_char input.[i] in
    for i = 0 to words - 1 do
      let x = get ((3 * i) + 0)
      and y = get ((3 * i) + 1)
      and z = get ((3 * i) + 2) in
      let n = (x lsl 16) lor (y lsl 8) lor z in
      let a = (n lsr 18) land 63
      and b = (n lsr 12) land 63
      and c = (n lsr 6) land 63
      and d = n land 63 in
      Bytes.set output ((4 * i) + 0) (to_char ?alphabet a) ;
      Bytes.set output ((4 * i) + 1) (to_char ?alphabet b) ;
      Bytes.set output ((4 * i) + 2) (to_char ?alphabet c) ;
      Bytes.set output ((4 * i) + 3) (to_char ?alphabet d)
    done ;
    for i = 1 to padding_len do
      Bytes.set output (Bytes.length output - i) padding
    done ;
    if pad then Bytes.unsafe_to_string output
    else Bytes.sub_string output 0 (Bytes.length output - padding_len)
end

let random len =
  let ic = open_in "/dev/urandom" in
  let rs = Bytes.create len in
  really_input ic rs 0 len ;
  close_in ic ;
  Bytes.unsafe_to_string rs

open Core
open Core_bench

let b64_encode_and_decode len =
  let input = random len in
  Staged.stage @@ fun () ->
  let encoded = Base64.encode_exn input in
  let _decoded = Base64.decode_exn encoded in
  ()

let b64c_encode_and_decode len =
  let input = random len in
  Staged.stage @@ fun () ->
  let encoded = Cstruct.of_string (Base64.encode_exn input) in
  let _decoded = Base64.decode_exn (Cstruct.to_string encoded) in
  ()

let old_encode_and_decode len =
  let input = random len in
  Staged.stage @@ fun () ->
  let encoded = Old_version.encode input in
  let _decoded = Old_version.decode encoded in
  ()

let mirage_crypto_encode_and_decode len =
  let input = random len in
  let input = Cstruct.of_string input in
  Staged.stage @@ fun () ->
  let encoded = Crypto_version.encode input in
  let _decoded = Crypto_version.decode encoded in
  ()

let args = [ 0; 10; 50; 100; 500; 1000; 2500; 5000 ]

let test_b64 =
  Bench.Test.create_indexed ~name:"Base64"
    ~args b64_encode_and_decode

let test_old =
  Bench.Test.create_indexed ~name:"Old"
    ~args old_encode_and_decode

let test_cur =
  Bench.Test.create_indexed ~name:"Crypto"
    ~args mirage_crypto_encode_and_decode

let test_b64c =
  Bench.Test.create_indexed ~name:"Base64 (cstruct)"
    ~args b64c_encode_and_decode

let command =
  Bench.make_command [ test_b64; test_old; test_cur; test_b64c ]

let () = Command.run command
