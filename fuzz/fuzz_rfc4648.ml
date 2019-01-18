open Crowbar

let pp_chr =
  let escaped = function ' ' .. '~' as c -> String.make 1 c | _ -> "." in
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
      if (i * 16) + !j < l then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp = pp_scalar ~get:String.get ~length:String.length

let (<.>) f g x = f (g x)

let char_from_alphabet alphabet : string gen =
  map [ range 64 ] (String.make 1 <.> Char.chr <.> Array.unsafe_get (B64.alphabet alphabet))

let random_string_from_alphabet alphabet len : string gen =
  let rec add_char_from_alphabet acc = function
  | 0 -> acc
  | n ->
      add_char_from_alphabet
        (concat_gen_list (const "") [ acc ; char_from_alphabet alphabet ])
        (n - 1) in
  add_char_from_alphabet (const "") len

let random_string_from_alphabet ~max alphabet =
  dynamic_bind (range max) (random_string_from_alphabet alphabet)

let encode_and_decode input =
  match B64.encode ~pad:true input with
  | Error (`Msg err) -> fail err
  | Ok result ->
      match B64.decode ~pad:true result with
      | Error (`Msg err) -> fail err
      | Ok result ->
          check_eq ~pp ~cmp:String.compare ~eq:String.equal result input

let decode_and_encode input =
  match B64.decode ~pad:true input with
  | Error (`Msg err) ->
      fail err
  | Ok result ->
      match B64.encode ~pad:true result with
      | Error (`Msg err) -> fail err
      | Ok result ->
          check_eq ~pp:Fmt.string ~cmp:String.compare ~eq:String.equal result input

let (//) x y =
  if y < 1 then raise Division_by_zero ;
  if x > 0 then 1 + ((x - 1) / y) else 0
[@@inline]

let canonic alphabet =
  let dmap = Array.make 256 (-1) in
  Array.iteri (fun i x -> Array.set dmap x i) (B64.alphabet alphabet) ;
  fun input ->
    let len = (String.length input // 4) * 4 in
    if len = String.length input then input
    else if len - String.length input = 3 then String.sub input 0 (String.length input - 1)
    else begin
      let rest = len - String.length input in
      let last = String.get input (String.length input - 1) in
      let output = Bytes.make len '=' in
      Bytes.blit_string input 0 output 0 (String.length input) ;
      let mask = match rest with
        | 1 -> 0x3c
        | 2 -> 0x30
        | _ -> assert false in
      let decoded = Array.get dmap (Char.code last) in
      let canonic = (decoded land mask) in
      let encoded = Array.get (B64.alphabet alphabet) canonic in
      Bytes.set output (String.length input - 1) (Char.chr encoded) ;
      Bytes.unsafe_to_string output
    end

let () =
  add_test ~name:"rfc4648: encode -> decode" [ bytes ] encode_and_decode ;
  add_test ~name:"rfc4648: encode -> decode" [ random_string_from_alphabet ~max:1000 B64.default_alphabet ] (decode_and_encode <.> canonic B64.default_alphabet)
