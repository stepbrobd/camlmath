open Camlmath

(* the five display blocks in pages/srd.md on ysun.co, the corpus this subset
   was sized for *)
let srd =
  [ ( "goroutine creation"
    , "\\frac{}{(G, M, C) \\xrightarrow{\\texttt{go f()}} (G \\cup \\{\\texttt{f()}\\}, \
       M, C)}" )
  ; ( "memory modification"
    , "\\frac{\\texttt{f()}\\ \\text{is}\\ \\texttt{data++}}{(G, M, C) \
       \\xrightarrow{\\texttt{go f()}} (G - \\{\\texttt{f()}\\}, M[\\texttt{data} \
       \\rightarrow M(\\texttt{data}) + 1], C)}" )
  ; ( "channel send"
    , "\\frac{\\texttt{f()}\\ \\text{is}\\ \\texttt{done <- true}}{(G, M, C) \
       \\xrightarrow{\\texttt{go f()}} (G - \\{\\texttt{f()}\\}, M, C[\\texttt{done} \
       \\rightarrow C(\\texttt{done}) \\cup \\{\\texttt{true}\\}])}" )
  ; ( "channel receive"
    , "\\frac{\\texttt{f()}\\ \\text{is}\\ \\texttt{<-done}}{(G, M, C) \
       \\xrightarrow{\\texttt{go f()}} (G - \\{\\texttt{f()}\\}, M, C[\\texttt{done} \
       \\rightarrow C(\\texttt{done}) - \\{\\texttt{true}\\}])}" )
  ; ( "print"
    , "\\frac{\\texttt{f()}\\ \\text{is}\\ \\texttt{fmt.Println(data)}}{(G, M, C) \
       \\xrightarrow{\\texttt{go f()}} (G \\cup \\{\\texttt{f()}\\}, M, C)}" )
  ]
;;

let conv ?display src =
  match to_mathml ?display src with
  | Ok m -> m
  | Error e -> Alcotest.failf "expected a conversion, got %a" pp_error e
;;

let err src =
  match to_mathml src with
  | Ok m -> Alcotest.failf "expected an error, got %s" m
  | Error e -> Format.asprintf "%a" pp_error e
;;

let contains haystack needle =
  let n = String.length needle in
  let rec go i =
    i + n <= String.length haystack
    && (String.equal (String.sub haystack i n) needle || go (i + 1))
  in
  go 0
;;

(* a tag balance walk, enough to catch an unclosed or crossed element without
   pulling in an xml parser *)
let well_formed s =
  let n = String.length s in
  let stack = ref [] in
  let i = ref 0 in
  let ok = ref true in
  while !ok && !i < n do
    if s.[!i] <> '<'
    then incr i
    else (
      let close = String.index_from s !i '>' in
      let body = String.sub s (!i + 1) (close - !i - 1) in
      let name buf =
        match String.index_opt buf ' ' with
        | Some k -> String.sub buf 0 k
        | None -> buf
      in
      if String.length body > 0 && body.[0] = '/'
      then (
        let want = String.sub body 1 (String.length body - 1) in
        match !stack with
        | top :: rest when String.equal top want -> stack := rest
        | _ -> ok := false)
      else if String.length body > 0 && body.[String.length body - 1] = '/'
      then ()
      else stack := name body :: !stack;
      i := close + 1)
  done;
  !ok && !stack = []
;;

let ascii_only s = String.for_all (fun c -> Char.code c < 0x80) s

let test_srd_converts () =
  List.iter
    (fun (label, src) ->
       ignore (conv src : string);
       ignore label)
    srd
;;

let test_srd_well_formed () =
  List.iter
    (fun (label, src) -> Alcotest.(check bool) label true (well_formed (conv src)))
    srd
;;

let test_srd_ascii () =
  List.iter
    (fun (label, src) -> Alcotest.(check bool) label true (ascii_only (conv src)))
    srd
;;

(* mathml core keeps mathvariant only as "normal" on <mi>. a converter that
   emits mathvariant="monospace" silently loses the face it asked for *)
let test_no_mathvariant () =
  List.iter
    (fun (label, src) ->
       Alcotest.(check bool) label false (contains (conv src) "mathvariant"))
    srd
;;

(* mpadded can move ink outside the box its parent reserves, and webkit honours
   that where blink clamps it *)
let test_no_mpadded () =
  List.iter
    (fun (label, src) ->
       Alcotest.(check bool) label false (contains (conv src) "mpadded"))
    srd
;;

let test_texttt_escapes_lt () =
  let m = conv "\\texttt{done <- true}" in
  Alcotest.(check bool) "escaped" true (contains m "done&#xa0;&lt;-&#xa0;true");
  Alcotest.(check bool) "well formed" true (well_formed m)
;;

let test_texttt_escapes_amp () =
  Alcotest.(check bool)
    "escaped"
    true
    (contains (conv "\\texttt{a & b}") "a&#xa0;&amp;&#xa0;b")
;;

let test_arrow_is_stretchy () =
  Alcotest.(check bool)
    "stretchy arrow"
    true
    (contains (conv "\\xrightarrow{\\texttt{go}}") "<mo stretchy=\"true\">&#x2192;</mo>")
;;

let test_empty_numerator () =
  Alcotest.(check bool)
    "empty mrow"
    true
    (contains (conv "\\frac{}{x}") "<mfrac><mrow />")
;;

let test_fences_do_not_stretch () =
  Alcotest.(check bool)
    "paren pinned"
    true
    (contains (conv "(x)") "<mo stretchy=\"false\">(</mo>")
;;

let test_minus_is_the_sign () =
  Alcotest.(check bool) "u+2212" true (contains (conv "a - b") "&#x2212;")
;;

let test_number_runs () =
  Alcotest.(check bool) "integer" true (contains (conv "123") "<mn>123</mn>");
  Alcotest.(check bool) "decimal" true (contains (conv "1.5") "<mn>1.5</mn>");
  Alcotest.(check bool)
    "trailing dot stays an operator"
    true
    (contains (conv "1.") "<mo>.</mo>")
;;

let test_scripts () =
  Alcotest.(check bool) "sub then sup" true (contains (conv "x_i^2") "<msubsup>");
  Alcotest.(check bool) "sup then sub" true (contains (conv "x^2_i") "<msubsup>");
  Alcotest.(check bool) "sub alone" true (contains (conv "x_i") "<msub>");
  Alcotest.(check bool) "sup alone" true (contains (conv "x^2") "<msup>")
;;

let test_display_modes () =
  Alcotest.(check bool) "block" true (contains (conv "x") "display=\"block\"");
  Alcotest.(check bool)
    "inline"
    false
    (contains (conv ~display:Inline "x") "display=\"block\"")
;;

let test_unknown_command_is_loud () =
  Alcotest.(check string) "reported" "unknown_command(\\sqrt at 0)" (err "\\sqrt{2}")
;;

let test_unknown_command_in_text_is_loud () =
  Alcotest.(check string) "reported" "unknown_command(\\foo at 8)" (err "\\texttt{\\foo}")
;;

let test_missing_argument_is_loud () =
  Alcotest.(check string) "reported" "missing_argument(\\frac at 0)" (err "\\frac{1}")
;;

let test_unclosed_group_is_loud () =
  Alcotest.(check string) "reported" "unclosed_group(opened at 0)" (err "{1 + 2")
;;

let test_stray_brace_is_loud () =
  Alcotest.(check string) "reported" "unexpected_token(} at 1)" (err "x}")
;;

let test_invalid_utf8_is_loud () =
  Alcotest.(check string) "reported" "invalid_utf8(at 1)" (err "x\xffy")
;;

(* xml's Char production is narrower than valid utf-8. these reach the emitter
   through text mode, the one path that copies source bytes into the output *)
let test_control_char_is_loud () =
  Alcotest.(check string) "form feed" "invalid_char(at 7)" (err "\\text{a\012b}")
;;

let test_noncharacter_is_loud () =
  Alcotest.(check string) "u+ffff" "invalid_char(at 7)" (err "\\text{a\239\191\191b}")
;;

let test_tab_and_newline_are_fine () =
  Alcotest.(check bool) "tab survives" true (contains (conv "\\text{a\tb}") "<mtext>")
;;

let test_vertical_bars () =
  Alcotest.(check bool)
    "bare bar is a fence"
    true
    (contains (conv "|x|") "<mo stretchy=\"false\">|</mo>");
  Alcotest.(check bool)
    "backslash bar is Vert"
    true
    (contains (conv "\\|x\\|") "&#x2016;");
  Alcotest.(check bool)
    "vert is a single bar"
    true
    (contains (conv "\\vert") "<mo stretchy=\"false\">|</mo>")
;;

(* tex sets uppercase greek upright, and math-auto would italicize it *)
let test_uppercase_greek_is_upright () =
  Alcotest.(check bool)
    "normal"
    true
    (contains (conv "\\Gamma") "<mi mathvariant=\"normal\">&#x393;</mi>");
  Alcotest.(check bool)
    "lowercase stays italic"
    true
    (contains (conv "\\gamma") "<mi>&#x3b3;</mi>")
;;

let test_epsilon_and_phi_glyphs () =
  Alcotest.(check bool) "epsilon is lunate" true (contains (conv "\\epsilon") "&#x3f5;");
  Alcotest.(check bool) "varepsilon" true (contains (conv "\\varepsilon") "&#x3b5;");
  Alcotest.(check bool) "phi is the symbol form" true (contains (conv "\\phi") "&#x3d5;");
  Alcotest.(check bool) "varphi" true (contains (conv "\\varphi") "&#x3c6;")
;;

(* an unbraced argument is one token, so the digit run must not swallow the
   denominator *)
let test_unbraced_argument () =
  Alcotest.(check bool)
    "one half"
    true
    (contains (conv "\\frac12") "<mfrac><mn>1</mn><mn>2</mn></mfrac>");
  Alcotest.(check bool)
    "braced still groups digits"
    true
    (contains (conv "\\frac{12}{3}") "<mfrac><mn>12</mn><mn>3</mn></mfrac>");
  Alcotest.(check string)
    "still loud when absent"
    "missing_argument(\\frac at 0)"
    (err "\\frac{1}")
;;

let test_aliases_and_escapes () =
  Alcotest.(check bool) "le" true (contains (conv "a \\le b") "&#x2264;");
  Alcotest.(check bool) "ne" true (contains (conv "a \\ne b") "&#x2260;");
  Alcotest.(check bool) "percent" true (contains (conv "50\\%") "<mo>%</mo>");
  Alcotest.(check bool)
    "ampersand escaped"
    true
    (contains (conv "a \\& b") "<mo>&amp;</mo>")
;;

let test_commands_are_listed () =
  Alcotest.(check bool) "frac listed" true (List.mem "frac" Parser.commands);
  Alcotest.(check bool) "sqrt absent" false (List.mem "sqrt" Parser.commands)
;;

let test_exn_entry_point () =
  let raised =
    try
      ignore (to_mathml_exn "\\sqrt{2}" : string);
      false
    with
    | Camlmath_error (Unknown_command ("sqrt", 0)) -> true
    | _ -> false
  in
  Alcotest.(check bool) "raises" true raised
;;

(* the exact markup ysun ships for pages/srd.md, one formula per line. a change
   here changes the site's bytes, so the output is pinned rather than merely
   checked for shape *)
let test_srd_golden () =
  let golden =
    In_channel.with_open_bin "srd.mathml" In_channel.input_all
    |> String.split_on_char '\n'
    |> List.filter (fun line -> line <> "")
  in
  Alcotest.(check int) "one line per formula" (List.length srd) (List.length golden);
  List.iter2
    (fun (label, src) want -> Alcotest.(check string) label want (conv src))
    srd
    golden
;;

(* a script argument is one token, as tex reads it, so x^12 is x to the first
   followed by a 2 and x^{12} is x to the twelfth *)
let test_script_argument_is_one_token () =
  Alcotest.(check bool)
    "unbraced"
    true
    (contains (conv "x^12") "<msup><mi>x</mi><mn>1</mn></msup><mn>2</mn>");
  Alcotest.(check bool)
    "braced"
    true
    (contains (conv "x^{12}") "<msup><mi>x</mi><mn>12</mn></msup>");
  Alcotest.(check bool)
    "command"
    true
    (contains (conv "x^\\alpha") "<msup><mi>x</mi><mi>&#x3b1;</mi></msup>");
  Alcotest.(check string)
    "still loud when absent"
    "unexpected_token(end of input at 2)"
    (err "x^")
;;

(* tex sets f' as f^{\prime} and joins a following ^ into the same superscript *)
let test_primes () =
  Alcotest.(check bool)
    "one"
    true
    (contains (conv "f'") "<msup><mi>f</mi><mo>&#x2032;</mo></msup>");
  Alcotest.(check bool) "two" true (contains (conv "f''") "<mo>&#x2033;</mo>");
  Alcotest.(check bool)
    "prime then power"
    true
    (contains
       (conv "f'^2")
       "<msup><mi>f</mi><mrow><mo>&#x2032;</mo><mn>2</mn></mrow></msup>");
  Alcotest.(check bool)
    "prime then subscript"
    true
    (contains (conv "f'_i") "<msubsup><mi>f</mi><mi>i</mi><mo>&#x2032;</mo></msubsup>");
  Alcotest.(check bool)
    "subscript then prime"
    true
    (contains (conv "x_i'") "<msubsup><mi>x</mi><mi>i</mi><mo>&#x2032;</mo></msubsup>");
  Alcotest.(check string) "double superscript" "unexpected_token(^ at 4)" (err "x'_i^2");
  Alcotest.(check string) "nothing before it" "unexpected_token(' at 0)" (err "'x")
;;

let case name f = Alcotest.test_case name `Quick f

let () =
  Alcotest.run
    "camlmath"
    [ ( "srd corpus"
      , [ case "converts" test_srd_converts
        ; case "well formed" test_srd_well_formed
        ; case "ascii only" test_srd_ascii
        ; case "no mathvariant" test_no_mathvariant
        ; case "no mpadded" test_no_mpadded
        ; case "golden" test_srd_golden
        ] )
    ; ( "emitter"
      , [ case "escapes < in text" test_texttt_escapes_lt
        ; case "escapes & in text" test_texttt_escapes_amp
        ; case "arrow stretches" test_arrow_is_stretchy
        ; case "empty numerator" test_empty_numerator
        ; case "fences pinned" test_fences_do_not_stretch
        ; case "minus sign" test_minus_is_the_sign
        ; case "display modes" test_display_modes
        ] )
    ; ( "parser"
      , [ case "number runs" test_number_runs
        ; case "scripts" test_scripts
        ; case "command list" test_commands_are_listed
        ; case "vertical bars" test_vertical_bars
        ; case "uppercase greek upright" test_uppercase_greek_is_upright
        ; case "epsilon and phi" test_epsilon_and_phi_glyphs
        ; case "unbraced argument" test_unbraced_argument
        ; case "aliases and escapes" test_aliases_and_escapes
        ; case "script argument" test_script_argument_is_one_token
        ; case "primes" test_primes
        ] )
    ; ( "failures"
      , [ case "unknown command" test_unknown_command_is_loud
        ; case "unknown command in text" test_unknown_command_in_text_is_loud
        ; case "missing argument" test_missing_argument_is_loud
        ; case "unclosed group" test_unclosed_group_is_loud
        ; case "stray brace" test_stray_brace_is_loud
        ; case "invalid utf-8" test_invalid_utf8_is_loud
        ; case "control character" test_control_char_is_loud
        ; case "noncharacter" test_noncharacter_is_loud
        ; case "tab and newline pass" test_tab_and_newline_are_fine
        ; case "exception entry point" test_exn_entry_point
        ] )
    ]
;;
