include Types
module Ast = Ast
module Lexer = Lexer
module Parser = Parser
module Emit = Emit

let parse = Parser.parse

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
