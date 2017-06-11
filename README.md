# Elm Color Mixing

Functions to mix color.

Use the [Elm Core Color type](http://package.elm-lang.org/packages/elm-lang/core/2.1.0/Color) to create your colors and this library to mix them.

This library is modeled off of the [LESSCSS color functions](http://lesscss.org/functions/#color-operations). See their documentation for general descriptions of each color operation.

## Working with elm-css

While elm-css do not use `Color` internally ([see issue 76](https://github.com/rtfeldman/elm-css/issues/76)).
You can use `Color.Mixing` with a small helper, like:

```elm
import Color.Mixing exposing (..)
import Color exposing (Color)
import Css

cssColor : Color -> Css.Color
cssColor color =
    let
        { red, green, blue, alpha } =
            Color.toRgb color
    in
        Css.rgba red green blue alpha
```
