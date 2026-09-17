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

- `\frac{a}{b}`, `\xrightarrow{label}`, `\xleftarrow{label}`. The optional
  argument that sets a second label under an arrow is refused rather than
  misread.
- `\text{...}` and `\texttt{...}`, in text mode where spaces are significant. An
  unescaped brace groups and vanishes as in TeX, `\{` `\}` `\$` `\%` `\&` `\#`
  `\_` are literal, `\` is a space and `\textbackslash` a backslash.
- `{...}` groups, `_` and `^` in either order, and a single unbraced token as an
  argument to `\frac`, the arrows, `_` and `^` alike, so `\frac12` is one half
  and `x^12` is x to the first followed by a 2, as TeX reads it
- primes: `f'` is `f^{\prime}`, `f''` uses the double prime glyph, and a `^`
  right after a prime joins the same superscript, so `f'^2` is `f^{\prime 2}`
- digit runs as one `<mn>`, letters as `<mi>`
- `(` `)` `[` `]` `|` pinned against stretching, `+` `-` `*`, and `=` `<` `>`
  `/` `,` `;` `!` `?` `.`
- `:` as the relation U+2236, which the operator dictionary spaces the way TeX
  spaces a relation, and `\colon` as the punctuation colon
- a sign after a relation, an opening fence or punctuation is wrapped with its
  operand, so `x = -1` has no gap after the minus, which is TeX's rule for a
  binary operator in that position
- `\,` `\:` `\;` `\quad` `\qquad` and the control space
- `\{` `\}` `\vert` for a single bar, `\|` and `\Vert` for a double one
- `\%` `\&` `\#` `\$` `\_`, the characters TeX reserves, set as identifiers
  because TeX gives them no operator spacing
- `\cup` `\cap` `\setminus` `\in` `\notin` `\subset` `\subseteq` `\times`
  `\cdot` `\pm`
- `\emptyset` `\infty` `\forall` `\exists` `\lnot`, ordinary symbols set as
  identifiers for the same reason
- `\leq` `\geq` `\neq` with their `\le` `\ge` `\ne` spellings, `\equiv`
  `\approx` `\land` `\lor` `\vdash` `\models` `\ldots` `\cdots`
- `\rightarrow` `\to` `\leftarrow` `\leftrightarrow` `\Rightarrow` `\mapsto`
- Greek letters, lowercase with the `\var` forms, and uppercase set upright the
  way TeX sets it

`Camlmath.Parser.commands` returns the same list at runtime.

## Failure

Anything outside the subset will result in an error.

```ocaml
type error =
  | Invalid_utf8 of int
  | Invalid_char of int
  | Unknown_command of string * int
  | Unexpected_char of string * int
  | Unexpected_token of string * int
  | Unclosed_group of int
  | Missing_argument of string * int
```

Every variant carries a byte offset, so `\sqrt{2}` reports
`unknown_command(\sqrt at 0)`. A character outside printable ASCII is named by
its code point, so a typed alpha reports `unexpected_char(U+03B1 at 0)`, and the
error strings are ASCII like the markup.

`Invalid_char` covers what XML cannot carry at all, as a literal or as a
reference: most C0 controls, and U+FFFE and U+FFFF. Valid UTF-8 is a wider set
than valid XML, so checking the encoding alone is not enough to keep the output
parseable. `Parser.parse` runs both checks itself, so the tree it returns can
always be emitted.
