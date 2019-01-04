module Tests exposing (all, colorEquality, colorEqualityDebug, startColor)

import Color exposing (..)
import Color.Mixing exposing (..)
import Debug
import Expect
import String
import Test exposing (..)


startColor =
    hsl (degrees 90) 0.8 0.5


colorEquality c1 c2 =
    Color.toCssString c1 == Color.toCssString c2


colorEqualityDebug c1 c2 =
    Debug.log "color1" (Color.toCssString c1) == Debug.log "color2" (Color.toCssString c2)


all : Test
all =
    describe
        "Elm Color Mixing Testing Suite!"
        [ test "Saturate" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (saturate 0.2 startColor)
                        (hsl (degrees 90) 1 0.5)
        , test "Desaturate" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (desaturate 0.2 startColor)
                        (hsl (degrees 90) 0.6 0.5)
        , test "Lighten" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (lighten 0.2 startColor)
                        (hsl (degrees 90) 0.8 0.7)
        , test "Darken" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (darken 0.2 startColor)
                        (hsl (degrees 90) 0.8 0.3)
        , test "FadeIn" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (fadeIn 0.1 (hsla (degrees 90) 0.8 0.5 0.8))
                        (hsla (degrees 90) 0.8 0.5 0.9)
        , test "FadeOut" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (fadeOut 0.1 (hsla (degrees 90) 0.8 0.5 0.8))
                        (hsla (degrees 90) 0.8 0.5 0.7)
        , test "Fade" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (fade 0.1 (hsla (degrees 90) 0.8 0.5 0.8))
                        (hsla (degrees 90) 0.8 0.5 0.1)
        , test "Spin" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (spin (degrees 30) (hsl (degrees 10) 0.9 0.5))
                        (hsl (degrees 40) 0.9 0.5)
        , test "Mix" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (mix 0.5 (rgba 100 0 0 1.0) (rgba 0 100 0 0.5))
                        (rgba 75 25 0 0.75)
        , test "Tint" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (tint 0.5 (rgba 0 0 255 0.5))
                        (rgba 191.25 191.25 255 0.75)
        , test "Shade" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (shade 0.5 (rgba 0 0 255 0.5))
                        (rgba 0 0 63.75 0.75)
        , test "Multiply" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (multiply (rgb 255 102 0) (rgb 0 0 0))
                        (rgb 0 0 0)
        , test "Softlight" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (softlight (rgb 255 102 0) (rgb 0 0 0))
                        (rgb 255 40.8 0)
        , test "Overlay" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (overlay (rgb 255 102 0) (rgb 0 0 0))
                        (rgb 255 0 0)
        , test "Overlay 2" <|
            \() ->
                Expect.true "" <|
                    colorEquality
                        (overlay (rgb 255 102 0) (rgb 51 51 51))
                        (rgb 255 40.8 0)
        ]
