# Changelog

## 2026.917.0

- A script argument is one token, as TeX reads it, so `x^12` is x to the first
  followed by a 2. `\frac`, the arrows, `_` and `^` share one argument reader.
- `'` is a superscript prime, `f''` the double prime glyph, and a `^` right
  after a prime joins the same superscript. It used to be a baseline `<mo>` that
  the operator dictionary spaced as an unknown operator.
- `\xrightarrow[a]{b}` is refused. The bracket used to become the label.
- `\emptyset`, `\forall`, `\exists`, `\lnot` and the reserved characters are
  identifiers. As operators outside the dictionary they took 0.2777em on each
  side, where TeX gives an ordinary symbol none.
- A sign after a relation, an opening fence or punctuation is wrapped with its
  operand, so `x = -1` has no gap after the minus.
- `:` is the relation U+2236, spaced as a binary operator rather than as
  punctuation, and `\colon` is the punctuation colon.
- Add `\varpi`, `\varrho` and `\varsigma`.
- Text mode follows TeX: an unescaped brace groups and vanishes, `\` is a space,
  `\textbackslash` a backslash, and `\\` is refused rather than read as a
  backslash. An unknown control symbol is named in the error.
- `Parser.parse` validates the source itself, so the public parser and emitter
  cannot produce a character XML refuses.
- `Unexpected_char` carries the character whole. An error names a character
  outside printable ASCII by its code point.
- `Ast.variant` is `Auto | Normal`. Its `Upright` shared a name with the text
  font in `Types`.
- The srd corpus output is pinned byte for byte in `test/srd.mathml`.

## 2026.903.1

- Reject code points outside XML's `Char` production. Valid UTF-8 admits C0
  controls and the U+FFFE and U+FFFF noncharacters, which no XML parser accepts,
  and `\text{}` copied them into the output.
- `\|` is `\Vert` and emits U+2016 rather than a single bar. `\vert` and a bare
  `|` are the single bar, and `|` no longer stops the conversion.
- Set uppercase Greek upright with `mathvariant="normal"`. MathML Core applies
  automatic italic to a single-character identifier, which TeX does not do to
  uppercase Greek.
- `\epsilon` and `\phi` emit the symbol forms U+03F5 and U+03D5, with
  `\varepsilon` and `\varphi` for U+03B5 and U+03C6. Complete the Greek table.
- Accept a single unbraced token as an argument, so `\frac12` is one half.
- Add `\le`, `\ge`, `\ne`, and the reserved characters `\%` `\&` `\#` `\$` `\_`.

## 2026.903.0

- Initial release. Converts fractions, extensible arrows, text runs, scripts,
  grouping, spacing, and a symbol and Greek letter table to MathML Core.
