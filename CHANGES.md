1.4 (10 September 2013)
-----------------------

- `FontData` can now be written back to SVG using `makeSvgFont` inside of the `WriteFont` module. 
  It is possible to only write back a specfic set of glyphs.
- The SVG `font-face` element is now completely supported. 
  This means all possible attributes are read and written back to it.
  Correct defaults are also set on non optional attributes. Some optional attributes are still 
  required though, due to their use in font rendering.
- The `ReadPath` module does not use `unsafePerformIO` anymore. `ReadFont` is now the only module with unsafe calls.
- Minor bug fixes:
  - `stemh` and `stemv` are now optional attributes.

1.3.0.2 (14 August 2013)
------------------------

- remove old comment causing Haddock build to fail

1.3.0.1 (12 August 2013)
------------------------

- fix repo location in .cabal file

1.3: 9 August 2013
------------------

- A bunch of bug fixes, cleanup, and reorganization:
  - Proper data type for `Kern` and `FontData` instead of mega-tuples.
  - Fixed several `Prelude.read` error on `font-face` attributes.
  - Fixed wrong attribute names.
  - Switched to `data-default-class` instead of `data-default`.
- Require `diagrams-lib-0.7`.
