module B64 = struct
  let encode ?pad ?alphabet input =
    B64.encode ?pad ?alphabet input

  let decode ?alphabet input =
    B64.decode ?alphabet input

  let alphabets = [B64.default_alphabet; B64.uri_safe_alphabet]

  let register_printer () =
    Printexc.register_printer
      (function
        | Not_found ->
          Some (Fmt.strf "(Decoding error: Character not found in alphabet)")
        | _ -> None)
end

module B64Fuzz = Fuzz.Make(B64)

let () = B64Fuzz.fuzz ()
