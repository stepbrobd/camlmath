open Types

type token =
  | Command of string
  | Lbrace
  | Rbrace
  | Sub
  | Sup
  | Char of char
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
        (* a control symbol is exactly one character, "\ " included *)
        t.pos <- at + 2;
        Command (String.make 1 t.src.[at + 1]), at)
      else (
        (* a backslash ending the source names no command. it is reported as an
           unknown one by the parser rather than dropped *)
        t.pos <- n;
        Command "", at)
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

let escapable = [ '{'; '}'; '\\'; '$'; '%'; '&'; '#'; '_' ]

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
      | '{' ->
        incr depth;
        Buffer.add_char buf '{';
        incr i
      | '}' ->
        decr depth;
        if !depth = 0 then closed := true else Buffer.add_char buf '}';
        incr i
      | '\\' when !i + 1 < n && List.mem t.src.[!i + 1] escapable ->
        Buffer.add_char buf t.src.[!i + 1];
        i := !i + 2
      | '\\' ->
        let j = ref (!i + 1) in
        while !j < n && is_letter t.src.[!j] do
          incr j
        done;
        err := Some (Unknown_command (String.sub t.src (!i + 1) (!j - !i - 1), !i))
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
