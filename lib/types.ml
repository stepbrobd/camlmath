type display =
  | Block
  | Inline

type font =
  | Upright
  | Monospace

type error =
  | Invalid_utf8 of int
  | Unknown_command of string * int
  | Unexpected_char of char * int
  | Unexpected_token of string * int
  | Unclosed_group of int
  | Missing_argument of string * int

exception Camlmath_error of error

let pp_error fmt = function
  | Invalid_utf8 at -> Format.fprintf fmt "invalid_utf8(at %d)" at
  | Unknown_command (name, at) -> Format.fprintf fmt "unknown_command(\\%s at %d)" name at
  | Unexpected_char (c, at) -> Format.fprintf fmt "unexpected_char(%C at %d)" c at
  | Unexpected_token (tok, at) -> Format.fprintf fmt "unexpected_token(%s at %d)" tok at
  | Unclosed_group at -> Format.fprintf fmt "unclosed_group(opened at %d)" at
  | Missing_argument (name, at) ->
    Format.fprintf fmt "missing_argument(\\%s at %d)" name at
;;
