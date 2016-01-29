module Color.Mixing (..) where

{-|

@docs Factor, darken, desaturate, fade, fadeIn, fadeOut, lighten, mix, saturate, spin, tint, shade, greyscale

@docs blend

@docs average, difference, exclusion, hardlight, multiply, negation, overlay, screen, softlight


-}

import Color exposing (..)


{-| -}
type alias Factor =
    Float


{-| -}
saturate : Factor -> Color -> Color
saturate x color =
    let
        colorHSL = toHsl color

        sat = clamp 0 1 (colorHSL.saturation + x)
    in
        hsla (colorHSL.hue) (sat) (colorHSL.lightness) (colorHSL.alpha)


{-| -}
desaturate : Factor -> Color -> Color
desaturate x color =
    let
        colorHSL = toHsl color

        sat = clamp 0 1 (colorHSL.saturation - x)
    in
        hsla (colorHSL.hue) (sat) (colorHSL.lightness) (colorHSL.alpha)


{-| -}
lighten : Factor -> Color -> Color
lighten x color =
    let
        colorHSL = toHsl color

        lightness = clamp 0 1 (colorHSL.lightness + x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (lightness) (colorHSL.alpha)


{-| -}
darken : Factor -> Color -> Color
darken x color =
    let
        colorHSL = toHsl color

        lightness = clamp 0 1 (colorHSL.lightness - x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (lightness) (colorHSL.alpha)


{-| -}
fadeIn : Factor -> Color -> Color
fadeIn x color =
    let
        colorHSL = toHsl color

        alpha = clamp 0 1 (colorHSL.alpha + x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)


{-| -}
fadeOut : Factor -> Color -> Color
fadeOut x color =
    let
        colorHSL = toHsl color

        alpha = clamp 0 1 (colorHSL.alpha - x)
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)


{-| -}
fade : Factor -> Color -> Color
fade x color =
    let
        colorHSL = toHsl color

        alpha = clamp 0 1 x
    in
        hsla (colorHSL.hue) (colorHSL.saturation) (colorHSL.lightness) (alpha)


{-| -}
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


{-| -}
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


{-| -}
greyscale : Color -> Color
greyscale color =
    desaturate 1.0 color


{-| -}
tint : Factor -> Color -> Color
tint x color =
    mix x (rgb 255 255 255) color


{-| -}
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


{-| -}
multiply : Color -> Color -> Color
multiply color1 color2 =
    blend (*) color1 color2


{-| -}
screen : Color -> Color -> Color
screen color1 color2 =
    blend (\c1 c2 -> c1 + c2 - c1 * c2) color1 color2


{-| -}
overlay : Color -> Color -> Color
overlay color1 color2 =
    blend
        (\c1 c2 ->
            let
                newc1 = c1 * 2
            in
                if newc1 <= 1 then
                    c1 * c2
                else
                    let
                        c1' = c1 - 1
                    in
                        c1' + c2 - c1' * c2
        )
        color1
        color2


{-| -}
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


{-| -}
hardlight : Color -> Color -> Color
hardlight color1 color2 =
    overlay color2 color1


{-| -}
difference : Color -> Color -> Color
difference color1 color2 =
    blend (\c1 c2 -> abs (c1 - c2)) color1 color2


{-| -}
exclusion : Color -> Color -> Color
exclusion color1 color2 =
    blend (\c1 c2 -> c1 + c2 - 2 * c1 * c2) color1 color2


{-| -}
average : Color -> Color -> Color
average color1 color2 =
    blend (\c1 c2 -> (c1 + c2) / 2) color1 color2


{-| -}
negation : Color -> Color -> Color
negation color1 color2 =
    blend (\c1 c2 -> 1 - abs (c1 + c2 - 1)) color1 color2
