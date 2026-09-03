(** The output tree. Every constructor maps to one MathML Core element.

    This is deliberately a MathML tree rather than a TeX tree. TeX constructs
    that have no MathML Core spelling are rejected in {!Parser} instead of being
    approximated here, so anything this type can hold can also be emitted.

    Two things are absent by construction, both of them defects observed in
    other converters:

    - There is no [mpadded]. Its [voffset] and [height] can move a box's ink
      outside the box its parent reserves, and WebKit honours that literally
      while Blink clamps it. Layout that needs padding uses {!Mspace}, which
      cannot express a negative metric.
    - There is no [mathvariant]. MathML Core keeps it only as ["normal"] on
      [<mi>], so a converter that emits [mathvariant="monospace"] loses the face
      it asked for. A monospace run is a {!Mtext} carrying {!Types.Monospace},
      which the emitter writes as a font family. *)

(** A fixed horizontal space. The widths are the TeX math spacing units, and
    none of them can be negative. *)
type space =
  | Thin (** [\,], 0.167em. *)
  | Medium (** [\:], 0.222em. *)
  | Thick (** [\;], 0.278em. *)
  | Interword (** [\ ], 0.333em. *)
  | Quad (** [\quad], 1em. *)
  | Qquad (** [\qquad], 2em. *)

(** Whether an operator resizes to its context. *)
type stretch =
  | Default (** Leave it to the operator dictionary. *)
  | Stretchy (** Force stretching, as an arrow under a label must. *)
  | Fixed
  (** Forbid stretching. A fence beside a tall sibling, such as a parenthesis
        next to a fraction, would otherwise grow to match it. *)

(** A MathML element. Character data is held unescaped and in UTF-8. The emitter
    is the only place that escapes it, so no caller can construct a node whose
    text escapes into markup. *)
type node =
  | Mi of string (** An identifier, italic by default. *)
  | Mn of string (** A number. *)
  | Mo of string * stretch (** An operator, fence or separator. *)
  | Mtext of Types.font * string (** A literal run, where spaces are significant. *)
  | Mspace of space
  | Mrow of node list
  (** A grouping. The empty row is legal and emits an empty element. *)
  | Mfrac of node * node (** Numerator, denominator. *)
  | Msub of node * node
  | Msup of node * node
  | Msubsup of node * node * node (** Base, subscript, superscript. *)
  | Munder of node * node (** Base, underscript. *)
  | Mover of node * node (** Base, overscript. *)

(** [row items] is [Mrow items], collapsed to the single item when there is
    exactly one. An empty list stays an empty {!Mrow}, which is what an empty
    [\frac] numerator needs. *)
val row : node list -> node
