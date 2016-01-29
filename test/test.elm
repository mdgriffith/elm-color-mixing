module Main (..) where

import String
import Graphics.Element exposing (Element)
import ElmTest exposing (..)
import Debug
import Color.Mixing exposing (..)
import Color exposing (..)


startColor =
    hsl (degrees 90) 0.8 0.5


colorEquality c1 c2 =
    let
        rgba1 = Debug.log "color1" <| toRgb c1

        rgba2 = Debug.log "color2" <| toRgb c2

        alphaTolerance a =
            round (a * 1000)
    in
        rgba1.red
            == rgba2.red
            && rgba1.green
            == rgba2.green
            && rgba1.blue
            == rgba2.blue
            && alphaTolerance rgba1.alpha
            == alphaTolerance rgba2.alpha


tests : Test
tests =
    suite
        "Elm Color Mixing Testing Suite!"
        [ test "Saturate"
            <| assert
            <| colorEquality
                (saturate 0.2 startColor)
                (hsl (degrees 90) 1 0.5)
        , test "Desaturate"
            <| assert
            <| colorEquality
                (desaturate 0.2 startColor)
                (hsl (degrees 90) 0.6 0.5)
        , test "Lighten"
            <| assert
            <| colorEquality
                (lighten 0.2 startColor)
                (hsl (degrees 90) 0.8 0.7)
        , test "Darken"
            <| assert
            <| colorEquality
                (darken 0.2 startColor)
                (hsl (degrees 90) 0.8 0.3)
        , test "FadeIn"
            <| assert
            <| colorEquality
                (fadeIn 0.1 (hsla (degrees 90) 0.8 0.5 0.8))
                (hsla (degrees 90) 0.8 0.5 0.9)
        , test "FadeOut"
            <| assert
            <| colorEquality
                (fadeOut 0.1 (hsla (degrees 90) 0.8 0.5 0.8))
                (hsla (degrees 90) 0.8 0.5 0.7)
        , test "Fade"
            <| assert
            <| colorEquality
                (fade 0.1 (hsla (degrees 90) 0.8 0.5 0.8))
                (hsla (degrees 90) 0.8 0.5 0.1)
        , test "Spin"
            <| assert
            <| colorEquality
                (spin (degrees 30) (hsl (degrees 10) 0.9 0.5))
                (hsl (degrees 40) 0.9 0.5)
        , test "Mix"
            <| assert
            <| colorEquality
                (mix 0.5 (rgba 100 0 0 1.0) (rgba 0 100 0 0.5))
                (rgba 75 25 0 0.75)
        , test "Tint"
            <| assert
            <| colorEquality
                (tint 0.5 (rgba 0 0 255 0.5))
                (rgba 191 191 255 0.75)
        , test "Shade"
            <| assert
            <| colorEquality
                (shade 0.5 (rgba 0 0 255 0.5))
                (rgba 0 0 64 0.75)
        ]


main : Element
main =
    elementRunner tests
