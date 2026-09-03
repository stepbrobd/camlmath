open Types
open Ast

let space_width = function
  | Thin -> "0.167em"
  | Medium -> "0.222em"
  | Thick -> "0.278em"
  | Interword -> "0.333em"
  | Quad -> "1em"
  | Qquad -> "2em"
;;

(* the single escaping path. [nbsp] is set for mtext only, where a run of
   ordinary spaces would otherwise collapse the way it does in html *)
let add_chardata ~nbsp b s =
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    let c = s.[!i] in
    if Char.code c < 0x80
    then (
      (match c with
       | '<' -> Buffer.add_string b "&lt;"
       | '>' -> Buffer.add_string b "&gt;"
       | '&' -> Buffer.add_string b "&amp;"
       | ' ' when nbsp -> Buffer.add_string b "&#xa0;"
       | c -> Buffer.add_char b c);
      incr i)
    else (
      (* validity is established once, at the entry point, so the decode here
         cannot fail *)
      let d = String.get_utf_8_uchar s !i in
      Buffer.add_string
        b
        (Printf.sprintf "&#x%x;" (Uchar.to_int (Uchar.utf_decode_uchar d)));
      i := !i + Uchar.utf_decode_length d)
  done
;;

let add_attr b name value =
  Buffer.add_char b ' ';
  Buffer.add_string b name;
  Buffer.add_string b "=\"";
  String.iter
    (function
      | '<' -> Buffer.add_string b "&lt;"
      | '&' -> Buffer.add_string b "&amp;"
      | '"' -> Buffer.add_string b "&quot;"
      | c -> Buffer.add_char b c)
    value;
  Buffer.add_char b '"'
;;

let rec add_node b node =
  match node with
  | Mi s -> leaf b "mi" [] s
  | Mn s -> leaf b "mn" [] s
  | Mo (s, Default) -> leaf b "mo" [] s
  | Mo (s, Stretchy) -> leaf b "mo" [ "stretchy", "true" ] s
  | Mo (s, Fixed) -> leaf b "mo" [ "stretchy", "false" ] s
  | Mtext (Upright, s) -> text b [] s
  | Mtext (Monospace, s) ->
    (* mathvariant is gone from mathml core, so the face is asked for the one
       way that survives: a font family on the element *)
    text b [ "style", "font-family:monospace" ] s
  | Mspace sp -> empty b "mspace" [ "width", space_width sp ]
  | Mrow [] -> empty b "mrow" []
  | Mrow items -> branch b "mrow" items
  | Mfrac (num, den) -> branch b "mfrac" [ num; den ]
  | Msub (base, sub) -> branch b "msub" [ base; sub ]
  | Msup (base, sup) -> branch b "msup" [ base; sup ]
  | Msubsup (base, sub, sup) -> branch b "msubsup" [ base; sub; sup ]
  | Munder (base, under) -> branch b "munder" [ base; under ]
  | Mover (base, over) -> branch b "mover" [ base; over ]

and open_tag b name attrs =
  Buffer.add_char b '<';
  Buffer.add_string b name;
  List.iter (fun (k, v) -> add_attr b k v) attrs

and close_tag b name =
  Buffer.add_string b "</";
  Buffer.add_string b name;
  Buffer.add_char b '>'

and empty b name attrs =
  open_tag b name attrs;
  Buffer.add_string b " />"

and leaf b name attrs s =
  open_tag b name attrs;
  Buffer.add_char b '>';
  add_chardata ~nbsp:false b s;
  close_tag b name

and text b attrs s =
  open_tag b "mtext" attrs;
  Buffer.add_char b '>';
  add_chardata ~nbsp:true b s;
  close_tag b "mtext"

and branch b name items =
  open_tag b name [];
  Buffer.add_char b '>';
  List.iter (add_node b) items;
  close_tag b name
;;

let fragment node =
  let b = Buffer.create 512 in
  add_node b node;
  Buffer.contents b
;;

let to_string ?(display = Block) node =
  let b = Buffer.create 512 in
  open_tag
    b
    "math"
    ([ "xmlns", "http://www.w3.org/1998/Math/MathML" ]
     @
     match display with
     | Block -> [ "display", "block" ]
     | Inline -> []);
  Buffer.add_char b '>';
  add_node b node;
  close_tag b "math";
  Buffer.contents b
;;
