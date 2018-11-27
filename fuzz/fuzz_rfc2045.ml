module Rfc2045 = struct
  exception Encode_error of string
  exception Decode_error

  let check_encode str =
    let subs = Astring.String.cuts ~sep:"\r\n" str in
    let check str =
      if String.length str > 78
      then
        raise (Encode_error "too long string returned")
    in List.iter check subs; str

  let encode ?pad:_ ?alphabet:_ input =
    let buf = Buffer.create 80 in
    let encoder = Rfc2045.encoder (`Buffer buf) in
    String.iter
      (fun c ->
        let ret = Rfc2045.encode encoder (`Char c) in
        match ret with
          | `Ok -> ()
          | _ -> raise (Encode_error "encoding was not Ok"))
      input;
    let encode = Rfc2045.encode encoder (`End) in
    match encode with
      | `Ok -> Buffer.contents buf |> check_encode
      | _ -> raise (Encode_error "encoding was not Ok")

  let decode ?alphabet:_ input =
    let decoder = Rfc2045.decoder (`String input) in
    let rec rec_decode acc =
      match Rfc2045.decode decoder with
        | `End -> acc
        | `Flush output -> rec_decode (Bytes.(of_string output |> copy |> to_string)::acc)
        | _ -> raise Decode_error
    in
     List.fold_left (^) "" (List.rev (rec_decode []))

  let alphabets = [""]

  let register_printer () =
    Printexc.register_printer
      (function
        | Encode_error err ->
          Some (Fmt.strf "(Encoding error: %s)" err)
        | Decode_error ->
          Some (Fmt.strf "(Decoding error: decoding was not Ok)")
        | _ -> None)
end

module Rfc2045Fuzz = Fuzz.Make(Rfc2045)

let () = Rfc2045Fuzz.fuzz ()
