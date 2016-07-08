port module Calliope
    exposing
        ( Project
        , defaultProject
        , defaultStructure
        , renderDialog
        , renderStructure
        , updateProject
        , Msg
        )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json
import Material.Grid as Grid exposing (..)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, when)


-- MODEL


type alias Project =
    { title : String
    , script : Script
    , structure : Structure
    }


type alias Script =
    { content : String
    }


type alias Scene =
    { name : String
    , location : String
    }


type alias Structure =
    { tierList : List Tier
    }


type alias Tier =
    { id : String
    , name : String
    }


type Msg
    = EditorReady



-- DEFAULTS


defaultProject : Project
defaultProject =
    { title = "New Project"
    , script =
        { content = "" }
    , structure = defaultStructure
    }


defaultStructure : Structure
defaultStructure =
    { tierList = [ { id = "Scenes", name = "Scenes" }, { id = "Scripts", name = "Scripts" } ] }



-- UPDATE


updateProject : Project -> String -> Project
updateProject project contentNew =
    { project | script = updateScript project.script contentNew }


updateScript : Script -> String -> Script
updateScript script contentNew =
    { script | content = contentNew }



-- RENDERING


renderStructure : Project -> Html a
renderStructure project =
    let
        gridWidth =
            widthFromTierList project.structure.tierList
    in
        Options.div [ css "background" "url('assets/bg.png')" ]
            [ Options.div
                (boxed 20 20)
                [ grid
                    ((boxed 12 0) ++ [ noSpacing ])
                  <|
                    List.append
                        (cellHeaders gridWidth project)
                        [ cellHeader gridWidth "Statistics" ]
                , grid
                    ((boxed 12 12)
                        ++ [ noSpacing
                           , Elevation.e6
                           , Color.background Color.white
                           ]
                    )
                    (cells gridWidth project)
                ]
            ]


cellHeaders : Int -> Project -> List (Cell a)
cellHeaders gridWidth project =
    List.map (cellHeaderFromTier gridWidth) project.structure.tierList


cellHeaderFromTier : Int -> Tier -> Cell a
cellHeaderFromTier gridWidth tier =
    cellHeader gridWidth tier.name


cellHeader : Int -> String -> Cell a
cellHeader gridWidth stHeader =
    cell
        [ Grid.size All gridWidth ]
        [ Options.styled Html.h5
            [ Color.text Color.accent ]
            [ text stHeader ]
        ]


widthFromTierList : List (Tier) -> Int
widthFromTierList tierList =
    let
        width =
            (//) 12 <| (+) 1 (List.length tierList)
    in
        if (width < 3) then
            3
        else
            width


cells : Int -> Project -> List (Cell a)
cells gridWidth project =
    List.map (cellFromTier gridWidth) project.structure.tierList


cellFromTier : Int -> Tier -> Cell a
cellFromTier gridWidth tier =
    cell
        [ Grid.size All gridWidth
        , css "height" "200px"
        ]
        [ text <| "Description of " ++ tier.name ++ " goes here!." ]


renderDialog : Project -> Bool -> Html Msg
renderDialog project refresh =
    Options.div [ css "background" "url('assets/bg.png')" ]
        [ Options.div
            (boxedDefault ++ [ css "max-width" "812px" ])
            [ Options.div
                [ Elevation.e6
                , css "height" "1024px"
                , css "position" "relative"
                , Color.background Color.white
                ]
                [ renderScript project.script refresh ]
            ]
        ]


renderScript : Script -> Bool -> Html Msg
renderScript script refresh =
    node "juicy-ace-editor"
        [ id "editor-container", on "editor-ready" (Json.succeed EditorReady) ]
        (if (refresh) then
            [ text script.content ]
         else
            []
        )


title : String -> Html a
title t =
    Options.styled Html.h1
        [ Color.text Color.primary ]
        [ text t ]


boxed : Int -> Int -> List (Options.Property a b)
boxed sides topBottom =
    let
        stSides =
            toString sides ++ "px"

        stTopBottom =
            toString topBottom ++ "px"
    in
        [ css "margin" "auto"
        , css "padding-left" stSides
        , css "padding-right" stSides
        , css "padding-top" stTopBottom
        , css "padding-bottom" stTopBottom
        ]


boxedDefault : List (Options.Property a b)
boxedDefault =
    boxed 80 20
