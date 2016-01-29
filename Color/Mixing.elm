module Color.Mixing where

{-|

@docs Factor, darken, desaturate, fade, fadeIn, fadeOut, lighten, mix, saturate, spin

@docs tint, shade, greyscale


-}

import Color exposing (..)

{-|-}
type alias Factor = Float

{-|-}
saturate : Factor -> Color -> Color
saturate x color = 
        let
          colorHSL = toHsl color
          sat = clamp 0 1 (colorHSL.saturation + x)
        in 
          hsla (colorHSL.hue) (sat) (colorHSL.lightness) (colorHSL.alpha)

{-|-}
desaturate : Factor -> Color -> Color
desaturate x color = 
        let
          colorHSL = toHsl color
          sat = clamp 0 1 (colorHSL.saturation - x)
        in 
          hsla (colorHSL.hue) (sat) (colorHSL.lightness) (colorHSL.alpha)

{-|-}
lighten : Factor -> Color -> Color
lighten x color = 
        let
          colorHSL = toHsl color
          lightness = clamp 0 1 (colorHSL.lightness + x)
        in 
          hsla (colorHSL.hue) (colorHSL.saturation) (lightness) (colorHSL.alpha)

{-|-}
darken : Factor -> Color -> Color
darken x color = 
        let
          colorHSL = toHsl color
          lightness = clamp 0 1 (colorHSL.lightness - x)
        in 
          hsla (colorHSL.hue) (colorHSL.saturation) (lightness) (colorHSL.alpha)

{-|-}
fadeIn : Factor -> Color -> Color
fadeIn x color = 
        let
          colorHSL = toHsl color
          alpha = clamp 0 1 (colorHSL.alpha + x)
        in 
          hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)

{-|-}
fadeOut : Factor -> Color -> Color
fadeOut x color = 
        let
          colorHSL = toHsl color
          alpha = clamp 0 1 (colorHSL.alpha - x)
        in 
          hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)

{-|-}
fade : Factor -> Color -> Color
fade x color = 
        let
          colorHSL = toHsl color
          alpha = clamp 0 1 x
        in 
          hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)

{-|-}
spin : Factor -> Color -> Color
spin x color = 
        let
          colorHSL = toHsl color
          protoHue = toFloat <| round (colorHSL.hue + x) % round 360.0
          hue =
            if protoHue < 0 then
              protoHue + 360
            else
              protoHue
        in 
          hsla hue (colorHSL.saturation) (colorHSL.lightness) (colorHSL.alpha)


{-|-}
mix : Factor -> Color -> Color -> Color
mix p color1 color2 = 
        let
          rgba1 = toRgb color1
          rgba2 = toRgb color2

          w = p * 2 - 1
          a = rgba1.alpha - rgba2.alpha

          w1 = 
            if w * a == -1 then
              w
            else
              (((w + a) / (1 + w * a)) + 1) / 2.0

          w2 = 1 - w1

          r = toFloat rgba1.red * w1 + toFloat rgba2.red * w2
          g = toFloat rgba1.green * w1 + toFloat rgba2.green * w2
          b = toFloat rgba1.blue * w1 + toFloat rgba2.blue * w2
          alpha = rgba1.alpha * p + rgba2.alpha * (1-p)
        in
          rgba (round r) (round g) (round b) alpha

{-|-}
greyscale : Color -> Color
greyscale color = desaturate 1.0 color

{-|-}
tint : Factor -> Color -> Color
tint x color = mix x (rgb 255 255 255) color 

{-|-}
shade : Factor -> Color -> Color
shade x color = mix x (rgb 0 0 0) color 




--multiply
--screen
--overlay
--softlight
--hardlight
--difference
--exclusion
--average
--negation