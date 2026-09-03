# camlmath

Binary Cache:

- Cache: <https://cache.ysun.co>
- Key: `cache.ysun.co-1:WxPYwT5g3kt9XhUhHPpNLZKI9HIOsVVAuqSHpok8Qt4=`

Converts a subset of TeX math to MathML Core markup.

Put this in dune:

```dune
(libraries camlmath)
```

API:

```ocaml
Camlmath.to_mathml : ?display:display -> string -> (string, error) result
Camlmath.parse     : string -> (Ast.node, error) result
```

`to_mathml_exn` raises `Camlmath_error` instead. The argument is the body of a
math block with the delimiters already stripped, and `display` defaults to
`Block`.

```ocaml
Camlmath.to_mathml_exn "\\frac{a}{b}"
(* <math xmlns="..." display="block"><mfrac><mi>a</mi><mi>b</mi></mfrac></math> *)
```

## Supported syntax

- `\frac{a}{b}`, `\xrightarrow{label}`, `\xleftarrow{label}`
- `\text{...}` and `\texttt{...}`, in text mode where spaces are significant
- `{...}` groups, `_` and `^` in either order
- digit runs as one `<mn>`, letters as `<mi>`
- `(` `)` `[` `]` pinned against stretching, `+` `-` `*`, and `=` `<` `>` `/`
  `,` `;` `:` `!` `?` `.` `'`
- `\,` `\:` `\;` `\quad` `\qquad` and the control space
- `\{` `\}` `\|`
- `\cup` `\cap` `\setminus` `\emptyset` `\in` `\notin` `\subset` `\subseteq`
  `\times` `\cdot` `\pm`
- `\leq` `\geq` `\neq` `\equiv` `\approx` `\land` `\lor` `\lnot` `\forall`
  `\exists` `\vdash` `\models` `\ldots` `\cdots` `\infty`
- `\rightarrow` `\to` `\leftarrow` `\leftrightarrow` `\Rightarrow` `\mapsto`
- Greek letters

`Camlmath.Parser.commands` returns the same list at runtime.

## Failure

Anything outside the subset will result in an error.

```ocaml
type error =
  | Invalid_utf8 of int
  | Unknown_command of string * int
  | Unexpected_char of char * int
  | Unexpected_token of string * int
  | Unclosed_group of int
  | Missing_argument of string * int
```

Every variant carries a byte offset, so `\sqrt{2}` reports
`unknown_command(\sqrt at 0)`.
