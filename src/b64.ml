(*
 * Copyright (c) 2006-2009 Citrix Systems Inc.
 * Copyright (c) 2010 Thomas Gazagnaire <thomas@gazagnaire.com>
 * Copyright (c) 2014-2016 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2016 David Kaloper Meršinjak
 * Copyright (c) 2018 Romain Calascibetta <romain.calascibetta@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

type alphabet =
  { emap : int array
  ; dmap : int array }

let (//) x y =
  if y < 1 then raise Division_by_zero ;
  if x > 0 then 1 + ((x - 1) / y) else 0
[@@inline]

let unsafe_get_uint8 t off = Char.code (String.unsafe_get t off)
let unsafe_set_uint8 t off v = Bytes.unsafe_set t off (Char.chr v)

external unsafe_set_uint16 : bytes -> int -> int -> unit = "%caml_string_set16u" [@@noalloc]
external unsafe_get_uint16 : string -> int -> int = "%caml_string_get16u" [@@noalloc]
external swap16 : int -> int = "%bswap16" [@@noalloc]

let none = (-1)
           let (<.>) f g = fun x -> f (g x)

let padding_exists alphabet = String.contains alphabet '='

let make_alphabet alphabet =
  if String.length alphabet <> 64 then invalid_arg "Length of alphabet must be 64" ;
  if padding_exists alphabet then invalid_arg "Alphabet can not contain padding character" ;
  let emap = Array.init (String.length alphabet) (Char.code <.> String.get alphabet)  in
  let dmap = Array.make 256 none in
  String.iteri (fun idx chr -> Array.set dmap (Char.code chr) idx) alphabet ;
  { emap; dmap; }

let length_alphabet { emap; _ } = Array.length emap

let default_alphabet = make_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
let uri_safe_alphabet = make_alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

let unsafe_set_be_uint16 =
  if Sys.big_endian
  then fun t off v -> unsafe_set_uint16 t off v
  else fun t off v -> unsafe_set_uint16 t off (swap16 v)

exception Out_of_bounds

let get_uint8 t off =
  if off < 0 || off >= String.length t then raise Out_of_bounds ;
  unsafe_get_uint8 t off

let padding = int_of_char '='

let encode pad { emap; _ } input =
  let n = String.length input in
  let n' = n // 3 * 4 in
  let res = Bytes.create n' in

  let emap i = Array.unsafe_get emap i in

  let emit b1 b2 b3 i =
    unsafe_set_be_uint16 res i
      ((emap (b1 lsr 2 land 0x3f) lsl 8)
       lor (emap ((b1 lsl 4) lor (b2 lsr 4) land 0x3f))) ;
    unsafe_set_be_uint16 res (i + 2)
      ((emap ((b2 lsl 2) lor (b3 lsr 6) land 0x3f) lsl 8)
       lor (emap (b3 land 0x3f))) in

  let rec enc j i =
    if i = n then ()
    else if i = n - 1 then emit (unsafe_get_uint8 input i) 0 0 j
    else if i = n - 2 then emit (unsafe_get_uint8 input i) (unsafe_get_uint8 input (i + 1)) 0 j
    else
    (emit
       (unsafe_get_uint8 input i)
       (unsafe_get_uint8 input (i + 1))
       (unsafe_get_uint8 input (i + 2))
       j ;
     enc (j + 4) (i + 3)) in

  let rec unsafe_fix = function
  | 0 -> ()
  | i -> unsafe_set_uint8 res (n' - i) padding ; unsafe_fix (i - 1) in

  enc 0 0 ;

  let padding = ((3 - n mod 3) mod 3) in

  if pad
  then begin unsafe_fix padding ; Bytes.unsafe_to_string res end
  else Bytes.sub_string res 0 (n' - padding)

let encode ?(pad = true) ?(alphabet = default_alphabet) input = encode pad alphabet input

let error_msgf fmt = Format.ksprintf (fun err -> Error (`Msg err)) fmt

let decode_result ?(pad = true) { dmap; _ } input =
  let n = (String.length input // 4) * 4 in
  let n' = (n // 4) * 3 in
  let res = Bytes.create n' in

  let get_uint8 t off =
    try get_uint8 t off with Out_of_bounds when not pad -> padding in

  let set_be_uint16 t off v =
    if off < 0 || off + 1 > Bytes.length t then ()
    else if off < 0 || off + 2 > Bytes.length t then unsafe_set_uint8 t off (v lsr 8)
    else unsafe_set_be_uint16 t off v in

  let set_uint8 t off v =
    if off < 0 || off >= Bytes.length t then ()
    else unsafe_set_uint8 t off v in

  let emit a b c d j =
    let x = (a lsl 18) lor (b lsl 12) lor (c lsl 6) lor d in
    set_be_uint16 res j (x lsr 8) ;
    set_uint8 res (j + 2) (x land 0xff) in

  let dmap i =
    let x = Array.unsafe_get dmap i in
    if x = none then raise Not_found ; x in

  let only_padding pad idx =

    (* because we round length of [res], we get only padding characters, we need
       to delete them, so for each [====], we delete 3 bytes. *)

    let pad = ref (pad + 3) in
    let idx = ref idx in
    let len = String.length input in
    while !idx + 4 < len do
      if unsafe_get_uint16 input !idx <> 0x3d3d || unsafe_get_uint16 input (!idx + 2) <> 0x3d3d then raise Not_found ;
      idx := !idx + 4 ;
      pad := !pad + 3 ;
    done ;
    while !idx < len do
      if unsafe_get_uint8 input !idx <> padding then raise Not_found ;
      incr idx ;
    done ; !pad in

  let rec dec j i =
    if i = n then 0
    else begin
      let (d, pad) =
        let x = get_uint8 input (i + 3) in
        try (dmap x, 0) with Not_found when x = padding -> (0, 1) in
      (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
      let (c, pad) =
        let x = get_uint8 input (i + 2) in
        try (dmap x, pad) with Not_found when x = padding && pad = 1 -> (0, 2) in
      (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
      let (b, pad) =
        let x = get_uint8 input (i + 1) in
        try (dmap x, pad) with Not_found when x = padding && pad = 2 -> (0, 3) in
      (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)
      let (a, pad) =
        let x = get_uint8 input i in
        try (dmap x, pad) with Not_found when x = padding && pad = 3 -> (0, 4) in
      (* [Not_found] iff [x ∉ alphabet and x <> '='] can leak. *)

      emit a b c d j ;

      if i + 4 = n
      (* end of input in anyway *)
      then match pad with
      | 0 -> 0
      | 4 -> 3
      (* [get_uint8] lies and if we get [4], that mean we got one or more (at
         most 4) padding character. In this situation, because we round length
         of [res] (see [n // 4]), we need to delete 3 bytes. *)
      | pad -> pad
      else match pad with
      | 0 -> dec (j + 3) (i + 4)
      | 4 -> only_padding 3 (i + 4)
      (* Same situation than above but we should get only more padding
         characters then. *)
      | pad -> only_padding pad (i + 4) end in

  match dec 0 0 with
  | 0 -> Ok (Bytes.unsafe_to_string res)
  | pad -> Ok (Bytes.sub_string res 0 (n' - pad))
  | exception Out_of_bounds -> error_msgf "Malformed input"
      (* only when [pad = true] and when length of input is not a multiple of 4. *)
  | exception Not_found ->
      (* appear when one character of [input] ∉ [alphabet] and this character <> '=' *)
      error_msgf "Malformed input"

let decode ?pad ?(alphabet = default_alphabet) input = decode_result ?pad alphabet input

let decode_opt ?pad ?alphabet input =
  match decode ?pad ?alphabet input with
  | Ok res -> Some res
  | Error _ -> None

let decode_exn ?pad ?alphabet input =
  match decode ?pad ?alphabet input with
  | Ok res -> res
  | Error (`Msg err) -> invalid_arg err
