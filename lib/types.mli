(** Types shared by the lexer, the parser and the emitter. *)

(** How the enclosing [<math>] element is laid out. *)
type display =
  | Block (** [$$...$$], emitted as [display="block"]. *)
  | Inline (** [$...$], the MathML default. *)

(** The face a text run is set in. *)
type font =
  | Upright (** [\text], the default face of the surrounding document. *)
  | Monospace (** [\texttt]. *)

(** Why a conversion failed. Every variant carries a byte offset into the source.

    There is no variant for a partial or degraded result. An expression either
    converts whole or reports where it stopped. *)
type error =
  | Invalid_utf8 of int (** The source is not well formed UTF-8. *)
  | Invalid_char of int
  (** A code point XML cannot carry, as a literal or as a reference. XML's
        [Char] production is narrower than "valid UTF-8": it excludes most C0
        controls and the noncharacters U+FFFE and U+FFFF. *)
  | Unknown_command of string * int
  (** A control sequence outside the supported subset. The string omits the
        leading backslash. *)
  | Unexpected_char of char * int (** A character with no meaning in math mode. *)
  | Unexpected_token of string * int (** A token in a position that has no reading. *)
  | Unclosed_group of int (** A group opened at this offset and never closed. *)
  | Missing_argument of string * int
  (** A command that requires a braced argument was not given one. *)

(** Raised by the [_exn] entry points in place of returning an error. *)
exception Camlmath_error of error

(** [pp_error] prints an error. The rendering is part of the interface, because
    a consumer converting at build time puts it in front of whoever wrote the
    expression. *)
val pp_error : Format.formatter -> error -> unit
