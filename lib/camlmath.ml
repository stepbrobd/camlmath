include Types
module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module Emit = Emit

(* xml's Char production is narrower than "valid utf-8". most c0 controls and
   the two noncharacters below u+10000 cannot appear in a document at all, as a
   literal or as a numeric reference *)
let xml_char u =
  u = 0x09
  || u = 0x0a
  || u = 0x0d
  || (u >= 0x20 && u <= 0xd7ff)
  || (u >= 0xe000 && u <= 0xfffd)
  || u >= 0x10000
;;

(* the emitter decodes utf-8 while escaping, and has no error path. both the
   encoding and the xml character range are established here instead, once, so
   that nothing downstream can emit a byte a parser will reject *)
let validate src =
  let n = String.length src in
  let rec go i =
    if i >= n
    then Ok ()
    else (
      let d = String.get_utf_8_uchar src i in
      if not (Uchar.utf_decode_is_valid d)
      then Error (Invalid_utf8 i)
      else if not (xml_char (Uchar.to_int (Uchar.utf_decode_uchar d)))
      then Error (Invalid_char i)
      else go (i + Uchar.utf_decode_length d))
  in
  go 0
;;

let parse src =
  match validate src with
  | Error e -> Error e
  | Ok () -> Parser.parse src
;;

let to_mathml ?display src =
  match parse src with
  | Error e -> Error e
  | Ok ast -> Ok (Emit.to_string ?display ast)
;;

let raise_or_return = function
  | Ok v -> v
  | Error e -> raise (Camlmath_error e)
;;

let to_mathml_exn ?display src = raise_or_return (to_mathml ?display src)
