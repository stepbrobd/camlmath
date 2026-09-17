open Types

type token =
  | Command of string
  | Lbrace
  | Rbrace
  | Sub
  | Sup
  | Char of char
  | Other of string
  | Eof

type t =
  { src : string
  ; mutable pos : int
  }

let make src = { src; pos = 0 }
let mark t = t.pos
let reset t at = t.pos <- at
let is_letter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
let is_space c = c = ' ' || c = '\t' || c = '\n' || c = '\r'

let rec skip_space t =
  if t.pos < String.length t.src && is_space t.src.[t.pos]
  then (
    t.pos <- t.pos + 1;
    skip_space t)
;;

(* the utf-8 character starting at byte [i], whole, so that an error can name
   it rather than its first byte *)
let char_at src i =
  let d = String.get_utf_8_uchar src i in
  String.sub src i (min (Uchar.utf_decode_length d) (String.length src - i))
;;

let next t =
  skip_space t;
  let n = String.length t.src in
  let at = t.pos in
  if at >= n
  then Eof, at
  else (
    match t.src.[at] with
    | '{' ->
      t.pos <- at + 1;
      Lbrace, at
    | '}' ->
      t.pos <- at + 1;
      Rbrace, at
    | '_' ->
      t.pos <- at + 1;
      Sub, at
    | '^' ->
      t.pos <- at + 1;
      Sup, at
    | '\\' ->
      let j = ref (at + 1) in
      while !j < n && is_letter t.src.[!j] do
        incr j
      done;
      if !j > at + 1
      then (
        (* a control word, and the trailing spaces it swallows are skipped by
           the next call rather than here *)
        t.pos <- !j;
        Command (String.sub t.src (at + 1) (!j - at - 1)), at)
      else if at + 1 < n
      then (
        (* a control symbol is exactly one character, "\ " included, and it is
           taken whole so that an unknown one is named whole *)
        let sym = char_at t.src (at + 1) in
        t.pos <- at + 1 + String.length sym;
        Command sym, at)
      else (
        (* a backslash ending the source names no command. it is reported as an
           unknown one by the parser rather than dropped *)
        t.pos <- n;
        Command "", at)
    | c when Char.code c >= 0x80 ->
      let s = char_at t.src at in
      t.pos <- at + String.length s;
      Other s, at
    | c ->
      t.pos <- at + 1;
      Char c, at)
;;

let peek t =
  let save = t.pos in
  let tok = next t in
  t.pos <- save;
  tok
;;

(* the characters a backslash makes literal in text mode. \\ is a line break in
   tex, not a backslash, so it is not among them *)
let escapable = [ '{'; '}'; '$'; '%'; '&'; '#'; '_' ]

let text_arg t ~cmd ~at =
  skip_space t;
  let n = String.length t.src in
  if t.pos >= n || t.src.[t.pos] <> '{'
  then Error (Missing_argument (cmd, at))
  else (
    let opened = t.pos in
    let buf = Buffer.create 32 in
    let i = ref (opened + 1) in
    let depth = ref 1 in
    let closed = ref false in
    let err = ref None in
    while (not !closed) && !err = None && !i < n do
      match t.src.[!i] with
      (* an unescaped brace groups, as in tex, and leaves no character *)
      | '{' ->
        incr depth;
        incr i
      | '}' ->
        decr depth;
        if !depth = 0 then closed := true;
        incr i
      | '\\' when !i + 1 < n && List.mem t.src.[!i + 1] escapable ->
        Buffer.add_char buf t.src.[!i + 1];
        i := !i + 2
      | '\\' when !i + 1 < n && t.src.[!i + 1] = ' ' ->
        (* the control space *)
        Buffer.add_char buf ' ';
        i := !i + 2
      | '\\' ->
        let j = ref (!i + 1) in
        while !j < n && is_letter t.src.[!j] do
          incr j
        done;
        if !j = !i + 1
        then (
          (* a control symbol outside the escapes above, named whole. a
             backslash ending the source names nothing *)
          let name = if !j < n then char_at t.src !j else "" in
          err := Some (Unknown_command (name, !i)))
        else (
          match String.sub t.src (!i + 1) (!j - !i - 1) with
          | "textbackslash" ->
            Buffer.add_char buf '\\';
            (* a control word swallows the spaces after it *)
            i := !j;
            while !i < n && is_space t.src.[!i] do
              incr i
            done
          | name -> err := Some (Unknown_command (name, !i)))
      | c ->
        Buffer.add_char buf c;
        incr i
    done;
    match !err with
    | Some e -> Error e
    | None ->
      if !closed
      then (
        t.pos <- !i;
        Ok (Buffer.contents buf))
      else Error (Unclosed_group opened))
;;

let describe = function
  | Command name -> "\\" ^ name
  | Lbrace -> "{"
  | Rbrace -> "}"
  | Sub -> "_"
  | Sup -> "^"
  | Char c -> String.make 1 c
  | Other s -> s
  | Eof -> "end of input"
;;

(* xml's Char production is narrower than "valid utf-8". most c0 controls and
   the two noncharacters below u+10000 cannot appear in a document at all, as a
   literal or as a numeric reference *)
let xml_char u =
  u = 0x09
  || u = 0x0a
  || u = 0x0d
  || (u >= 0x20 && u <= 0xd7ff)
  || (u >= 0xe000 && u <= 0xfffd)
  || u >= 0x10000
;;

(* the emitter decodes utf-8 while escaping, and has no error path. both the
   encoding and the xml character range are established here instead, once,
   so that nothing downstream can emit a byte a parser will reject *)
let validate src =
  let n = String.length src in
  let rec go i =
    if i >= n
    then Ok ()
    else (
      let d = String.get_utf_8_uchar src i in
      if not (Uchar.utf_decode_is_valid d)
      then Error (Invalid_utf8 i)
      else if not (xml_char (Uchar.to_int (Uchar.utf_decode_uchar d)))
      then Error (Invalid_char i)
      else go (i + Uchar.utf_decode_length d))
  in
  go 0
;;
