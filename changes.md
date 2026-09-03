# Changelog

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
