(** TeX tokenization.

    The lexer is a cursor over the source. Math mode and text mode are different
    languages, so they are different entry points: {!next} tokenizes math mode
    and discards whitespace, while {!text_arg} reads a braced argument verbatim
    because a space inside [\texttt{go f()}] is part of the content. *)

type token =
  | Command of string (** A control sequence, without its backslash. *)
  | Lbrace
  | Rbrace
  | Sub (** [_] *)
  | Sup (** [^] *)
  | Char of char
  | Eof

type t

(** [make src] starts a cursor at the beginning of [src]. *)
val make : string -> t

(** [next t] consumes and returns the next math-mode token with its offset.
    Whitespace before the token is skipped, as TeX does in math mode. *)
val next : t -> token * int

(** [peek t] is {!next} without consuming. *)
val peek : t -> token * int

(** [mark t] records the cursor, for the one place the grammar needs to look
    further ahead than {!peek} allows. *)
val mark : t -> int

(** [reset t at] returns the cursor to a position from {!mark}. *)
val reset : t -> int -> unit

(** [text_arg t ~cmd ~at] reads the braced argument of [cmd], which starts at
    [at], as text-mode content. Spaces are preserved, brace nesting is tracked,
    and a backslash escapes one of the TeX special characters. Any other
    control sequence inside text mode is an error rather than a silent drop. *)
val text_arg : t -> cmd:string -> at:int -> (string, Types.error) result

(** [describe tok] names a token the way an error message shows it: the
    character itself, the control sequence with its backslash, or
    ["end of input"]. *)
val describe : token -> string
