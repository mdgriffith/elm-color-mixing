module Color.Mixing (Factor, lighten, darken, saturate, desaturate, tint, shade, fade, fadeIn, fadeOut, mix, spin, average, difference, multiply, exclusion, negation, overlay, screen, softlight, hardlight) where

{-|

@docs Factor, lighten, darken, saturate, desaturate, fade, fadeIn, fadeOut,  mix, spin, tint, shade

@docs average, difference, exclusion, hardlight, multiply, negation, overlay, screen, softlight


-}

import Color exposing (..)


{-| A Float that should be between 0.0 and 1.0

-}
type alias Factor =
    Float


{-| Increase the saturation of a color in the HSL color space by an absolute amount.

-}
saturate : Factor -> Color -> Color
saturate x color =
    let
        colorHSL = toHsl color

        sat = clamp 0 1 (colorHSL.saturation + x)
    in
        hsla (colorHSL.hue) (sat) (colorHSL.lightness) (colorHSL.alpha)


{-| Decrease the saturation of a color in the HSL color space by an absolute amount.

-}
desaturate : Factor -> Color -> Color
desaturate x color =
    let
        colorHSL = toHsl color

        sat = clamp 0 1 (colorHSL.saturation - x)
    in
        hsla (colorHSL.hue) (sat) (colorHSL.lightness) (colorHSL.alpha)


{-| Increase the lightness of a color.
-}
lighten : Factor -> Color -> Color
lighten x color =
    let
        colorHSL = toHsl color

        lightness = clamp 0 1 (colorHSL.lightness + x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (lightness) (colorHSL.alpha)


{-| Decrease the lightness of a color.
-}
darken : Factor -> Color -> Color
darken x color =
    let
        colorHSL = toHsl color

        lightness = clamp 0 1 (colorHSL.lightness - x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (lightness) (colorHSL.alpha)


{-| Decrease the transparency of a color, making it more opaque.
-}
fadeIn : Factor -> Color -> Color
fadeIn x color =
    let
        colorHSL = toHsl color

        alpha = clamp 0 1 (colorHSL.alpha + x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)


{-| Increase the transparency of a color, making it less opaque.
-}
fadeOut : Factor -> Color -> Color
fadeOut x color =
    let
        colorHSL = toHsl color

        alpha = clamp 0 1 (colorHSL.alpha - x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)


{-| Set the absolute transparency of a color.
-}
fade : Factor -> Color -> Color
fade x color =
    let
        colorHSL = toHsl color

        alpha = clamp 0 1 x
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)


{-| Rotate the hue angle of a color in either direction.

-}
spin : Factor -> Color -> Color
spin x color =
    let
        colorHSL = toHsl color

        precision = 1000000.0

        protoHue = (toFloat <| round ((colorHSL.hue + x) * precision) % round ((degrees 360.0) * precision)) / precision

        hue =
            if protoHue < 0 then
                protoHue + (degrees 360.0)
            else
                protoHue
    in
        hsla hue (colorHSL.saturation) (colorHSL.lightness) (colorHSL.alpha)


{-| Mix two colors together in variable proportion. Opacity is included in the calculations.
-}
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

        alpha = rgba1.alpha * p + rgba2.alpha * (1 - p)
    in
        rgba (round r) (round g) (round b) alpha


{-| Mix color with white in variable proportion. Same as calling `mix` with white.
-}
tint : Factor -> Color -> Color
tint x color =
    mix x (rgb 255 255 255) color


{-| Mix color with black in variable proportion.  Same as calling `mix` with black.
-}
shade : Factor -> Color -> Color
shade x color =
    mix x (rgb 0 0 0) color


{-| -}
blend : (Float -> Float -> Float) -> Color -> Color -> Color
blend fn color1 color2 =
    let
        rgba1 = toRgb color1

        rgba2 = toRgb color2

        newAlpha = rgba2.alpha + rgba1.alpha * (1 - rgba2.alpha)

        apply channel1 channel2 =
            let
                c1 = toFloat channel1 / 255

                c2 = toFloat channel2 / 255

                c' = fn c1 c2

                newChannel =
                    if newAlpha /= 0.0 then
                        (rgba2.alpha
                            * c2
                            + rgba1.alpha
                            * (c1
                                - rgba2.alpha
                                * (c1 + c2 - c')
                              )
                        )
                            / newAlpha
                    else
                        c'
            in
                round <| newChannel * 255
    in
        rgba
            (apply rgba1.red rgba2.red)
            (apply rgba1.green rgba2.green)
            (apply rgba1.blue rgba2.blue)
            newAlpha


{-| Multiply two colors. Corresponding RGB channels from each of the two colors are multiplied together then divided by 255. The result is a darker color.
-}
multiply : Color -> Color -> Color
multiply color1 color2 =
    blend (*) color1 color2


{-| Do the opposite of `multiply`. The result is a brighter color.
-}
screen : Color -> Color -> Color
screen color1 color2 =
    blend (\c1 c2 -> c1 + c2 - c1 * c2) color1 color2


{-| Combines the effects of both multiply and screen. Conditionally make light channels lighter and dark channels darker.

__Note:__ The results of the conditions are determined by the first color parameter.

-}
overlay : Color -> Color -> Color
overlay color1 color2 =
    blend
        (\c1 c2 ->
            let
                newc1 = c1 * 2
            in
                if newc1 <= 1 then
                    newc1 * c2
                else
                    let
                        c1' = newc1 - 1
                    in
                        c1' + c2 - c1' * c2
        )
        color1
        color2


{-| Similar to overlay but avoids pure black resulting in pure black, and pure white resulting in pure white.

-}
softlight : Color -> Color -> Color
softlight color1 color2 =
    blend
        (\c1 c2 ->
            let
                ( e, d ) =
                    if (c2 > 0.5) then
                        let
                            d =
                                if c1 > 0.25 then
                                    sqrt c1
                                else
                                    ((16 * c1 - 12) * c1 + 4) * c1
                        in
                            ( d, 1 )
                    else
                        ( c1, 1 )
            in
                c1 - (1 - 2 * c2) * e * (d - c1)
        )
        color1
        color2


{-| The same as overlay but with the color roles reversed.

-}
hardlight : Color -> Color -> Color
hardlight color1 color2 =
    overlay color2 color1


{-| Subtracts the second color from the first color on a channel-by-channel basis. Negative values are inverted. Subtracting black results in no change; subtracting white results in color inversion.
-}
difference : Color -> Color -> Color
difference color1 color2 =
    blend (\c1 c2 -> abs (c1 - c2)) color1 color2


{-| A similar effect to difference with lower contrast.
-}
exclusion : Color -> Color -> Color
exclusion color1 color2 =
    blend (\c1 c2 -> c1 + c2 - 2 * c1 * c2) color1 color2


{-| Compute the average of two colors on a per-channel (RGB) basis.
-}
average : Color -> Color -> Color
average color1 color2 =
    blend (\c1 c2 -> (c1 + c2) / 2) color1 color2


{-| Do the opposite effect to difference.
-}
negation : Color -> Color -> Color
negation color1 color2 =
    blend (\c1 c2 -> 1 - abs (c1 + c2 - 1)) color1 color2
