type display =
  | Block
  | Inline

type font =
  | Upright
  | Monospace

type error =
  | Invalid_utf8 of int
  | Invalid_char of int
  | Unknown_command of string * int
  | Unexpected_char of string * int
  | Unexpected_token of string * int
  | Unclosed_group of int
  | Missing_argument of string * int

exception Camlmath_error of error

(* error strings stay ascii like the markup. a printable ascii character is
   shown as it is, anything else as its code point *)
let pp_name fmt s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let d = String.get_utf_8_uchar s !i in
    let u = Uchar.to_int (Uchar.utf_decode_uchar d) in
    if u >= 0x21 && u <= 0x7e
    then Format.pp_print_char fmt (Char.chr u)
    else Format.fprintf fmt "U+%04X" u;
    i := !i + Uchar.utf_decode_length d
  done
;;

let pp_error fmt = function
  | Invalid_utf8 at -> Format.fprintf fmt "invalid_utf8(at %d)" at
  | Invalid_char at -> Format.fprintf fmt "invalid_char(at %d)" at
  | Unknown_command (name, at) ->
    Format.fprintf fmt "unknown_command(\\%a at %d)" pp_name name at
  | Unexpected_char (c, at) -> Format.fprintf fmt "unexpected_char(%a at %d)" pp_name c at
  | Unexpected_token (tok, at) -> Format.fprintf fmt "unexpected_token(%s at %d)" tok at
  | Unclosed_group at -> Format.fprintf fmt "unclosed_group(opened at %d)" at
  | Missing_argument (name, at) ->
    Format.fprintf fmt "missing_argument(\\%s at %d)" name at
;;
