open Types
open Ast

let ( let* ) = Result.bind

(* symbols carry no arguments. an entry maps a control sequence to the single
   node it stands for *)
let symbols =
  [ "cup", Mo ("\u{222a}", Default)
  ; "cap", Mo ("\u{2229}", Default)
  ; "setminus", Mo ("\u{2216}", Default)
  ; "emptyset", Mo ("\u{2205}", Default)
  ; "in", Mo ("\u{2208}", Default)
  ; "notin", Mo ("\u{2209}", Default)
  ; "subset", Mo ("\u{2282}", Default)
  ; "subseteq", Mo ("\u{2286}", Default)
  ; "rightarrow", Mo ("\u{2192}", Default)
  ; "to", Mo ("\u{2192}", Default)
  ; "leftarrow", Mo ("\u{2190}", Default)
  ; "leftrightarrow", Mo ("\u{2194}", Default)
  ; "Rightarrow", Mo ("\u{21d2}", Default)
  ; "mapsto", Mo ("\u{21a6}", Default)
  ; "times", Mo ("\u{00d7}", Default)
  ; "cdot", Mo ("\u{22c5}", Default)
  ; "pm", Mo ("\u{00b1}", Default)
  ; "leq", Mo ("\u{2264}", Default)
  ; "geq", Mo ("\u{2265}", Default)
  ; "neq", Mo ("\u{2260}", Default)
  ; "equiv", Mo ("\u{2261}", Default)
  ; "approx", Mo ("\u{2248}", Default)
  ; "land", Mo ("\u{2227}", Default)
  ; "lor", Mo ("\u{2228}", Default)
  ; "lnot", Mo ("\u{00ac}", Default)
  ; "forall", Mo ("\u{2200}", Default)
  ; "exists", Mo ("\u{2203}", Default)
  ; "vdash", Mo ("\u{22a2}", Default)
  ; "models", Mo ("\u{22a8}", Default)
  ; "ldots", Mo ("\u{2026}", Default)
  ; "cdots", Mo ("\u{22ef}", Default)
  ; "infty", Mi "\u{221e}"
  ; "alpha", Mi "\u{03b1}"
  ; "beta", Mi "\u{03b2}"
  ; "gamma", Mi "\u{03b3}"
  ; "delta", Mi "\u{03b4}"
  ; "epsilon", Mi "\u{03b5}"
  ; "lambda", Mi "\u{03bb}"
  ; "mu", Mi "\u{03bc}"
  ; "pi", Mi "\u{03c0}"
  ; "rho", Mi "\u{03c1}"
  ; "sigma", Mi "\u{03c3}"
  ; "tau", Mi "\u{03c4}"
  ; "phi", Mi "\u{03c6}"
  ; "psi", Mi "\u{03c8}"
  ; "omega", Mi "\u{03c9}"
  ; "Gamma", Mi "\u{0393}"
  ; "Delta", Mi "\u{0394}"
  ; "Lambda", Mi "\u{039b}"
  ; "Sigma", Mi "\u{03a3}"
  ; "Phi", Mi "\u{03a6}"
  ; "Omega", Mi "\u{03a9}"
    (* a brace reaches math mode escaped. it is a fence, so it must not stretch
       to a tall sibling any more than a parenthesis does *)
  ; "{", Mo ("{", Fixed)
  ; "}", Mo ("}", Fixed)
  ; "|", Mo ("|", Fixed)
  ; ",", Mspace Thin
  ; ":", Mspace Medium
  ; ";", Mspace Thick
  ; " ", Mspace Interword
  ; "quad", Mspace Quad
  ; "qquad", Mspace Qquad
  ]
;;

let with_args = [ "frac"; "xrightarrow"; "xleftarrow"; "text"; "texttt" ]
let commands = with_args @ List.map fst symbols
let is_digit c = c >= '0' && c <= '9'
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

(* a run of digits is one <mn>, and a decimal point joins the run only when a
   digit follows it. that needs one token more of lookahead than peek gives *)
let number lx first =
  let buf = Buffer.create 8 in
  Buffer.add_char buf first;
  let rec loop () =
    match Lexer.peek lx with
    | Lexer.Char c, _ when is_digit c ->
      ignore (Lexer.next lx);
      Buffer.add_char buf c;
      loop ()
    | Lexer.Char '.', _ ->
      let save = Lexer.mark lx in
      ignore (Lexer.next lx);
      (match Lexer.peek lx with
       | Lexer.Char d, _ when is_digit d ->
         Buffer.add_char buf '.';
         loop ()
       | _ -> Lexer.reset lx save)
    | _ -> ()
  in
  loop ();
  Mn (Buffer.contents buf)
;;

let char_atom lx c at =
  if is_digit c
  then Ok (number lx c)
  else if is_letter c
  then Ok (Mi (String.make 1 c))
  else (
    match c with
    | '(' | ')' | '[' | ']' -> Ok (Mo (String.make 1 c, Fixed))
    | '+' -> Ok (Mo ("+", Default))
    | '-' -> Ok (Mo ("\u{2212}", Default))
    | '*' -> Ok (Mo ("\u{2217}", Default))
    | '=' | '<' | '>' | '/' | ',' | ';' | ':' | '!' | '?' | '.' | '\'' ->
      Ok (Mo (String.make 1 c, Default))
    | _ -> Error (Unexpected_char (c, at)))
;;

let rec parse_row lx =
  let rec loop acc =
    match Lexer.peek lx with
    | (Lexer.Rbrace | Lexer.Eof), _ -> Ok (List.rev acc)
    | _ ->
      let* item = parse_script lx in
      loop (item :: acc)
  in
  loop []

and parse_script lx =
  let* base = parse_atom lx in
  attach lx base

and attach lx base =
  match Lexer.peek lx with
  | Lexer.Sub, _ ->
    ignore (Lexer.next lx);
    let* sub = parse_atom lx in
    (match Lexer.peek lx with
     | Lexer.Sup, _ ->
       ignore (Lexer.next lx);
       let* sup = parse_atom lx in
       Ok (Msubsup (base, sub, sup))
     | _ -> Ok (Msub (base, sub)))
  | Lexer.Sup, _ ->
    ignore (Lexer.next lx);
    let* sup = parse_atom lx in
    (match Lexer.peek lx with
     | Lexer.Sub, _ ->
       ignore (Lexer.next lx);
       let* sub = parse_atom lx in
       Ok (Msubsup (base, sub, sup))
     | _ -> Ok (Msup (base, sup)))
  | _ -> Ok base

and parse_atom lx =
  let tok, at = Lexer.next lx in
  match tok with
  | Lexer.Lbrace ->
    let* items = parse_row lx in
    (match Lexer.next lx with
     | Lexer.Rbrace, _ -> Ok (row items)
     | _ -> Error (Unclosed_group at))
  | Lexer.Rbrace -> Error (Unexpected_token ("}", at))
  | Lexer.Sub -> Error (Unexpected_token ("_", at))
  | Lexer.Sup -> Error (Unexpected_token ("^", at))
  | Lexer.Eof -> Error (Unexpected_token ("end of input", at))
  | Lexer.Char c -> char_atom lx c at
  | Lexer.Command name -> command lx name at

and group lx ~cmd ~at =
  match Lexer.peek lx with
  | Lexer.Lbrace, opened ->
    ignore (Lexer.next lx);
    let* items = parse_row lx in
    (match Lexer.next lx with
     | Lexer.Rbrace, _ -> Ok (row items)
     | _ -> Error (Unclosed_group opened))
  | _ -> Error (Missing_argument (cmd, at))

and command lx name at =
  match name with
  | "frac" ->
    let* num = group lx ~cmd:name ~at in
    let* den = group lx ~cmd:name ~at in
    Ok (Mfrac (num, den))
  | "xrightarrow" -> extensible lx name at "\u{2192}"
  | "xleftarrow" -> extensible lx name at "\u{2190}"
  | "text" ->
    let* s = Lexer.text_arg lx ~cmd:name ~at in
    Ok (Mtext (Upright, s))
  | "texttt" ->
    let* s = Lexer.text_arg lx ~cmd:name ~at in
    Ok (Mtext (Monospace, s))
  | _ ->
    (match List.assoc_opt name symbols with
     | Some node -> Ok node
     | None -> Error (Unknown_command (name, at)))

(* an arrow carrying a label. the arrow is forced stretchy so it spans the
   label, and the label is padded with real space on both sides. a converter
   that instead shrinks the label box with a negative mpadded height leaves the
   ink outside the box, which webkit draws on top of the arrow *)
and extensible lx cmd at glyph =
  let* label = group lx ~cmd ~at in
  Ok (Mover (Mo (glyph, Stretchy), Mrow [ Mspace Thick; label; Mspace Thick ]))
;;

let parse src =
  let lx = Lexer.make src in
  let* items = parse_row lx in
  match Lexer.next lx with
  | Lexer.Eof, _ -> Ok (row items)
  | Lexer.Rbrace, at -> Error (Unexpected_token ("}", at))
  | _, at -> Error (Unexpected_token ("token", at))
;;
