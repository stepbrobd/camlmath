open Types
open Ast

let ( let* ) = Result.bind

(* symbols carry no arguments. an entry maps a control sequence to the single
   node it stands for *)
let symbols =
  [ "cup", Mo ("\u{222a}", Default)
  ; "cap", Mo ("\u{2229}", Default)
  ; "setminus", Mo ("\u{2216}", Default)
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
  ; "le", Mo ("\u{2264}", Default)
  ; "geq", Mo ("\u{2265}", Default)
  ; "ge", Mo ("\u{2265}", Default)
  ; "neq", Mo ("\u{2260}", Default)
  ; "ne", Mo ("\u{2260}", Default)
  ; "equiv", Mo ("\u{2261}", Default)
  ; "approx", Mo ("\u{2248}", Default)
  ; "land", Mo ("\u{2227}", Default)
  ; "lor", Mo ("\u{2228}", Default)
  ; "vdash", Mo ("\u{22a2}", Default)
  ; "models", Mo ("\u{22a8}", Default)
  ; "ldots", Mo ("\u{2026}", Default)
  ; "cdots", Mo ("\u{22ef}", Default)
    (* ordinary symbols in tex, class ord, so identifiers rather than operators.
       an <mo> the operator dictionary does not list takes 0.2777em on each
       side, and math-auto has no italic form for any of these *)
  ; "infty", Mi ("\u{221e}", Italic)
  ; "emptyset", Mi ("\u{2205}", Italic)
  ; "lnot", Mi ("\u{00ac}", Italic)
  ; "forall", Mi ("\u{2200}", Italic)
  ; "exists", Mi ("\u{2203}", Italic)
    (* lowercase greek is italic in tex, which is also what math-auto gives a
       single character, so these carry no variant *)
  ; "alpha", Mi ("\u{03b1}", Italic)
  ; "beta", Mi ("\u{03b2}", Italic)
  ; "gamma", Mi ("\u{03b3}", Italic)
  ; "delta", Mi ("\u{03b4}", Italic)
  ; "epsilon", Mi ("\u{03f5}", Italic)
  ; "varepsilon", Mi ("\u{03b5}", Italic)
  ; "zeta", Mi ("\u{03b6}", Italic)
  ; "eta", Mi ("\u{03b7}", Italic)
  ; "theta", Mi ("\u{03b8}", Italic)
  ; "vartheta", Mi ("\u{03d1}", Italic)
  ; "iota", Mi ("\u{03b9}", Italic)
  ; "kappa", Mi ("\u{03ba}", Italic)
  ; "lambda", Mi ("\u{03bb}", Italic)
  ; "mu", Mi ("\u{03bc}", Italic)
  ; "nu", Mi ("\u{03bd}", Italic)
  ; "xi", Mi ("\u{03be}", Italic)
  ; "pi", Mi ("\u{03c0}", Italic)
  ; "rho", Mi ("\u{03c1}", Italic)
  ; "sigma", Mi ("\u{03c3}", Italic)
  ; "tau", Mi ("\u{03c4}", Italic)
  ; "upsilon", Mi ("\u{03c5}", Italic)
  ; "phi", Mi ("\u{03d5}", Italic)
  ; "varphi", Mi ("\u{03c6}", Italic)
  ; "chi", Mi ("\u{03c7}", Italic)
  ; "psi", Mi ("\u{03c8}", Italic)
  ; "omega", Mi ("\u{03c9}", Italic)
    (* tex sets uppercase greek upright. math-auto would italicize it, and
       mathvariant="normal" is the one spelling mathml core kept to say no *)
  ; "Gamma", Mi ("\u{0393}", Upright)
  ; "Delta", Mi ("\u{0394}", Upright)
  ; "Theta", Mi ("\u{0398}", Upright)
  ; "Lambda", Mi ("\u{039b}", Upright)
  ; "Xi", Mi ("\u{039e}", Upright)
  ; "Pi", Mi ("\u{03a0}", Upright)
  ; "Sigma", Mi ("\u{03a3}", Upright)
  ; "Upsilon", Mi ("\u{03a5}", Upright)
  ; "Phi", Mi ("\u{03a6}", Upright)
  ; "Psi", Mi ("\u{03a8}", Upright)
  ; "Omega", Mi ("\u{03a9}", Upright)
    (* a fence reaching math mode escaped must not stretch to a tall sibling any
       more than a parenthesis does. \| is \Vert, not a single bar *)
  ; "{", Mo ("{", Fixed)
  ; "}", Mo ("}", Fixed)
  ; "vert", Mo ("|", Fixed)
  ; "|", Mo ("\u{2016}", Fixed)
  ; "Vert", Mo ("\u{2016}", Fixed)
    (* the characters tex reserves, reachable in math mode only escaped. class
       ord as well, and as operators the percent sign would take a gap *)
  ; "%", Mi ("%", Italic)
  ; "&", Mi ("&", Italic)
  ; "#", Mi ("#", Italic)
  ; "$", Mi ("$", Italic)
  ; "_", Mi ("_", Italic)
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
  then Ok (Mi (String.make 1 c, Italic))
  else (
    match c with
    | '(' | ')' | '[' | ']' | '|' -> Ok (Mo (String.make 1 c, Fixed))
    | '+' -> Ok (Mo ("+", Default))
    | '-' -> Ok (Mo ("\u{2212}", Default))
    | '*' -> Ok (Mo ("\u{2217}", Default))
    | '=' | '<' | '>' | '/' | ',' | ';' | ':' | '!' | '?' | '.' ->
      Ok (Mo (String.make 1 c, Default))
    (* a prime is a superscript on the atom before it, read by [attach]. one
       with nothing before it has no reading *)
    | '\'' -> Error (Unexpected_token ("'", at))
    | _ -> Error (Unexpected_char (c, at)))
;;

(* tex sets a run of primes as one glyph *)
let prime_glyph = function
  | 1 -> "\u{2032}"
  | 2 -> "\u{2033}"
  | 3 -> "\u{2034}"
  | 4 -> "\u{2057}"
  | n -> String.concat "" (List.init n (fun _ -> "\u{2032}"))
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

(* scripts in either order, at most one of each. a superscript may also come
   from a run of primes, so x_i' and x'^2_i read the way tex reads them *)
and attach lx base =
  let* sub = subscript lx in
  let* sup = superscript lx in
  let* sub =
    match sub with
    | Some _ -> Ok sub
    | None -> subscript lx
  in
  Ok
    (match sub, sup with
     | None, None -> base
     | Some sub, None -> Msub (base, sub)
     | None, Some sup -> Msup (base, sup)
     | Some sub, Some sup -> Msubsup (base, sub, sup))

and subscript lx =
  match Lexer.peek lx with
  | Lexer.Sub, _ ->
    ignore (Lexer.next lx);
    let* arg = script_arg lx in
    Ok (Some arg)
  | _ -> Ok None

(* a ^ right after a run of primes joins the same superscript, which is what
   tex's prime macro does so that f'^2 is f^{\prime 2} *)
and superscript lx =
  match primes lx 0 with
  | 0 ->
    (match Lexer.peek lx with
     | Lexer.Sup, _ ->
       ignore (Lexer.next lx);
       let* arg = script_arg lx in
       Ok (Some arg)
     | _ -> Ok None)
  | n ->
    let prime = Mo (prime_glyph n, Default) in
    (match Lexer.peek lx with
     | Lexer.Sup, _ ->
       ignore (Lexer.next lx);
       let* arg = script_arg lx in
       Ok (Some (Mrow [ prime; arg ]))
     | _ -> Ok (Some prime))

and primes lx n =
  match Lexer.peek lx with
  | Lexer.Char '\'', _ ->
    ignore (Lexer.next lx);
    primes lx (n + 1)
  | _ -> n

and script_arg lx = argument lx ~missing:(fun tok at -> Unexpected_token (tok, at))

(* an argument is a braced row or one token, the rule tex applies to \frac, to
   the extensible arrows and to _ and ^ alike. [missing] builds the error when
   the input or the enclosing group ends instead *)
and argument lx ~missing =
  match Lexer.peek lx with
  | Lexer.Lbrace, opened ->
    ignore (Lexer.next lx);
    braced lx opened
  | ((Lexer.Eof | Lexer.Rbrace) as tok), at -> Error (missing (Lexer.describe tok) at)
  | _ -> single lx

(* the row after an opening brace already consumed, through its closing brace *)
and braced lx opened =
  let* items = parse_row lx in
  match Lexer.next lx with
  | Lexer.Rbrace, _ -> Ok (row items)
  | _ -> Error (Unclosed_group opened)

and parse_atom lx =
  let tok, at = Lexer.next lx in
  match tok with
  | Lexer.Lbrace -> braced lx at
  | Lexer.Char c -> char_atom lx c at
  | Lexer.Command name -> command lx name at
  | tok -> Error (Unexpected_token (Lexer.describe tok, at))

(* an unbraced argument is one token, so \frac12 is one half and x^12 is x to
   the first followed by a 2. a digit must not pull in the digits after it the
   way an ordinary atom does *)
and single lx =
  let tok, at = Lexer.next lx in
  match tok with
  | Lexer.Char c when is_digit c -> Ok (Mn (String.make 1 c))
  | Lexer.Char c -> char_atom lx c at
  | Lexer.Command name -> command lx name at
  | tok -> Error (Unexpected_token (Lexer.describe tok, at))

and command lx name at =
  let arg () = argument lx ~missing:(fun _ _ -> Missing_argument (name, at)) in
  match name with
  | "frac" ->
    let* num = arg () in
    let* den = arg () in
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
  match Lexer.peek lx with
  (* amsmath's optional argument sets a second label under the arrow, which is
     outside the subset. read as an unbraced argument it would become the label
     itself, so it is refused rather than misread *)
  | Lexer.Char '[', at -> Error (Unexpected_token ("[", at))
  | _ ->
    let* label = argument lx ~missing:(fun _ _ -> Missing_argument (cmd, at)) in
    Ok (Mover (Mo (glyph, Stretchy), Mrow [ Mspace Thick; label; Mspace Thick ]))
;;

let parse src =
  let lx = Lexer.make src in
  let* items = parse_row lx in
  match Lexer.next lx with
  | Lexer.Eof, _ -> Ok (row items)
  | tok, at -> Error (Unexpected_token (Lexer.describe tok, at))
;;
