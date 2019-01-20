(*
 * Copyright (c) 2016 Anil Madhavapeddy <anil@recoil.org>
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

open Printf
open Rresult

(* Test vectors from RFC4648
   BASE64("") = ""
   BASE64("f") = "Zg=="
   BASE64("fo") = "Zm8="
   BASE64("foo") = "Zm9v"
   BASE64("foob") = "Zm9vYg=="
   BASE64("fooba") = "Zm9vYmE="
   BASE64("foobar") = "Zm9vYmFy"
*)

let rfc4648_tests = [
  "", "";
  "f", "Zg==";
  "fo", "Zm8=";
  "foo", "Zm9v";
  "foob", "Zm9vYg==";
  "fooba", "Zm9vYmE=";
  "foobar", "Zm9vYmFy";
]

let hannes_tests = [
  "dummy", "ZHVtbXk=";
  "dummy", "ZHVtbXk";
  "dummy", "ZHVtbXk==";
  "dummy", "ZHVtbXk===";
  "dummy", "ZHVtbXk====";
  "dummy", "ZHVtbXk=====";
  "dummy", "ZHVtbXk======";
]

let php_tests = [
  "πάντα χωρεῖ καὶ οὐδὲν μένει …", "z4DOrM69z4TOsSDPh8-Jz4HOteG_liDOus6x4b22IM6_4b2QzrThvbLOvSDOvM6tzr3Otc65IOKApg"
]

let rfc3548_tests = [
  "\x14\xfb\x9c\x03\xd9\x7e", "FPucA9l+";
  "\x14\xfb\x9c\x03\xd9", "FPucA9k=";
  "\x14\xfb\x9c\x03", "FPucAw==";
]

let cfcs_tests = [
  0, 2, "\004", "BB";
  1, 2, "\004", "ABB";
  1, 2, "\004", "ABBA";
  2, 2, "\004", "AABBA";
  2, 2, "\004", "AABBAA";
  0, 0, "", "BB";
  1, 0, "", "BB";
  2, 0, "", "BB";
]

let alphabet_size () =
  List.iter (fun (name,alphabet) ->
    Alcotest.(check int) (sprintf "Alphabet size %s = 64" name)
     64 (Base64.length_alphabet alphabet))
     ["default",Base64.default_alphabet; "uri_safe",Base64.uri_safe_alphabet]

(* Encode using OpenSSL `base64` utility *)
let openssl_encode buf =
  Bos.(OS.Cmd.in_string buf |> OS.Cmd.run_io (Cmd.v "base64") |> OS.Cmd.to_string ~trim:true) |>
  function | Ok r -> prerr_endline r; r | Error (`Msg e) -> raise (Failure (sprintf "OpenSSL decode: %s" e))

(* Encode using this library *)
let lib_encode buf =
  Base64.encode_exn ~pad:true buf

let test_rfc4648 () =
  List.iter (fun (c,r) ->
    (* Base64 vs openssl *)
    Alcotest.(check string) (sprintf "encode %s" c) (openssl_encode c) (lib_encode c);
    (* Base64 vs test cases above *)
    Alcotest.(check string) (sprintf "encode rfc4648 %s" c) r (lib_encode c);
    (* Base64 decode vs library *)
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn r);
  ) rfc4648_tests

let test_rfc3548 () =
  List.iter (fun (c,r) ->
    (* Base64 vs openssl *)
    Alcotest.(check string) (sprintf "encode %s" c) (openssl_encode c) (lib_encode c);
    (* Base64 vs test cases above *)
    Alcotest.(check string) (sprintf "encode rfc3548 %s" c) r (lib_encode c);
    (* Base64 decode vs library *)
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn r);
  ) rfc3548_tests

let test_hannes () =
  List.iter (fun (c,r) ->
    (* Base64 vs test cases above *)
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn ~pad:false r);
  ) hannes_tests

let test_php () =
  List.iter (fun (c,r) ->
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn ~pad:false ~alphabet:Base64.uri_safe_alphabet r);
  ) php_tests

let test_cfcs () =
  List.iter (fun (off, len, c,r) ->
    Alcotest.(check string) (sprintf "decode %s" r) c (Base64.decode_exn ~pad:false ~off ~len r);
  ) cfcs_tests



let test_invariants = [ "Alphabet size", `Quick, alphabet_size ]
let test_codec = [ "RFC4648 test vectors", `Quick, test_rfc4648
                 ; "RFC3548 test vectors", `Quick, test_rfc3548
                 ; "Hannes test vectors", `Quick, test_hannes
                 ; "Cfcs test vectors", `Quick, test_cfcs
                 ; "PHP test vectors", `Quick, test_php ]

let () =
  Alcotest.run "Base64" [
    "invariants", test_invariants;
    "codec", test_codec;
  ]

