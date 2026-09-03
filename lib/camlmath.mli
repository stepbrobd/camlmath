(** Converts a subset of TeX math to MathML Core markup.

    The conversion is a pure function of the source string. Nothing is read from
    the environment, no process is spawned, and no font is consulted, so the
    same input gives the same bytes on every machine.

    {1 What it will not do}

    The subset is closed. A control sequence outside {!Parser.commands} is
    {!Types.Unknown_command}, not a best effort, and not the source echoed back
    as text. A caller that wants a fallback writes it, visibly.

    {1 What the output avoids}

    Two habits inherited from MathJax-era converters are absent by construction,
    see {!Ast} for the reasoning:

    - no [mpadded] with negative metrics, which WebKit and Blink lay out
      differently;
    - no [mathvariant], which MathML Core reduced to ["normal"] on [<mi>].

    Output is ASCII, so it can be pasted into a source file of any encoding, and
    it carries no named entity beyond the five XML defines, so an XML validity
    gate accepts it. *)

include module type of Types

(** [to_mathml ?display src] converts one expression. [src] is the body of a
    math block with the delimiters already stripped. [display] defaults to
    {!Block}. *)
val to_mathml : ?display:display -> string -> (string, error) result

(** [to_mathml_exn] is {!to_mathml}, raising {!Camlmath_error} instead of
    returning an error. *)
val to_mathml_exn : ?display:display -> string -> string

(** [parse src] stops at the syntax tree, for a caller that emits markup itself
    or inspects what an expression uses. *)
val parse : string -> (Ast.node, error) result

(** The MathML tree the parser produces and the emitter consumes. *)
module Ast = Ast

(** TeX tokenization. *)
module Lexer = Lexer

(** TeX to {!Ast}, and the list of accepted control sequences. *)
module Parser = Parser

(** {!Ast} to markup. *)
module Emit = Emit
