(** {!Ast} to MathML markup.

    This module owns every byte of output, which is what keeps the markup well
    formed. Character data reaches the buffer through one escaping function, so
    a [<] inside [\texttt{done <- true}] cannot escape into markup and produce
    the [StartTag: invalid element name] that an XML validity gate reports.

    Output is pure ASCII. A character above U+007F is written as a numeric
    reference, never as a named entity, because XML defines only five entity
    names and a named reference outside them is a validity error. *)

(** [to_string ?display node] renders a complete [<math>] element. [display]
    defaults to {!Types.Block}. *)
val to_string : ?display:Types.display -> Ast.node -> string

(** [fragment node] renders [node] without the enclosing [<math>], for a caller
    assembling markup itself. *)
val fragment : Ast.node -> string
