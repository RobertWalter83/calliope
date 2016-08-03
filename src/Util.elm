module Util exposing (boxed, boxedDefault, withMaxWidth)

import Material.Options as Options exposing (css, when)


boxed : (Int, Int) -> List (Options.Property a b)
boxed (paddingSides, paddingTopBottom) =
    let
        stSides =
            toString paddingSides ++ "px"

        stTopBottom =
            toString paddingTopBottom ++ "px"
    in
        [ css "margin" "auto"
        , css "padding-left" stSides
        , css "padding-right" stSides
        , css "padding-top" stTopBottom
        , css "padding-bottom" stTopBottom
        ]

withMaxWidth : Int -> List (Options.Property a b) -> List (Options.Property a b)
withMaxWidth maxWidth cssProps =
    cssProps ++ [ css "max-width"  (toString maxWidth ++ "px") ]

boxedDefault : List (Options.Property a b)
boxedDefault =
    boxed (80, 20) 