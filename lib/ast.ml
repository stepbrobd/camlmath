type space =
  | Thin
  | Medium
  | Thick
  | Interword
  | Quad
  | Qquad

type variant =
  | Italic
  | Upright

type stretch =
  | Default
  | Stretchy
  | Fixed

type node =
  | Mi of string * variant
  | Mn of string
  | Mo of string * stretch
  | Mtext of Types.font * string
  | Mspace of space
  | Mrow of node list
  | Mfrac of node * node
  | Msub of node * node
  | Msup of node * node
  | Msubsup of node * node * node
  | Munder of node * node
  | Mover of node * node

let row = function
  | [ single ] -> single
  | items -> Mrow items
;;
