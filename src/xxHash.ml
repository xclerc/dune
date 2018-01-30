type t = int64

external file : string -> (t [@unboxed])
  = "jbuilder_digest_file_byte" "jbuilder_digest_file" [@@noalloc]

external string : string -> (t [@unboxed])
  = "jbuilder_digest_string_byte" "jbuilder_digest_string" [@@noalloc]

let hex_char n =
  let n =
    if n < 10 then
      n + Char.code '0'
    else
      n + Char.code 'a' - 10
  in
  Char.unsafe_chr n

let to_hex t =
  let result = Bytes.create 16 in
  for i = 0 to 7 do
    let x = Int64.to_int (Int64.shift_right_logical t (i lsl 3)) in
    let j = i lsl 1 in
    Bytes.unsafe_set result  j      (hex_char ((x lsr 4) land 0x0f));
    Bytes.unsafe_set result (j + 1) (hex_char ( x        land 0x0f));
  done;
  Bytes.unsafe_to_string result

let decode_hex_char c =
  match c with
  | '0'..'9' -> Char.code c - Char.code '0'
  | 'A'..'F' -> Char.code c - Char.code 'A' + 10
  | 'a'..'f' -> Char.code c - Char.code 'a' + 10
  | _ -> invalid_arg "XxHash.of_hex"

let of_hex s =
  if String.length s <> 16 then invalid_arg "XxHash.of_hex";
  let result = ref 0L in
  for i = 0 to 7 do
    let n =
      let j = i lsl 1 in
      decode_hex_char s.[j    ] lsl 4 +
      decode_hex_char s.[j + 1]
    in
    result := Int64.add !result (Int64.shift_left (Int64.of_int n) (i lsl 3))
  done;
  !result
