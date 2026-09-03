include Types
module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module Emit = Emit

(* the emitter decodes utf-8 while escaping, and has no error path. validity is
   established here instead, once, so that decode cannot fail downstream *)
let validate src =
  let n = String.length src in
  let rec go i =
    if i >= n
    then Ok ()
    else (
      let d = String.get_utf_8_uchar src i in
      if Uchar.utf_decode_is_valid d
      then go (i + Uchar.utf_decode_length d)
      else Error (Invalid_utf8 i))
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
