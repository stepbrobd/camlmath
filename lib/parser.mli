(** TeX to {!Ast}.

    The supported subset is closed and small: fractions, the extensible arrows,
    text runs, scripts, grouping, spacing, and the symbol and letter tables in
    {!commands}. Anything else is {!Types.Unknown_command}. There is no fallback
    that emits the source back as text, because a build that silently ships raw
    TeX is worse than one that stops. *)

(** [parse src] converts one expression. [src] is the body of a math block, with
    the surrounding [$$] already stripped. *)
val parse : string -> (Ast.node, Types.error) result

(** [commands] lists every control sequence the parser accepts, without the
    leading backslash and in no particular order. Exposed so a consumer can
    report the subset rather than guess at it. *)
val commands : string list
