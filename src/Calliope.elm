module Calliope
    exposing
        ( Project
        , defaultProject
        , createProject
        , defaultStructure
        , renderDialog
        , renderStructure
        , updateProject
        , encodeProject
        , Msg
        )

import Date exposing (..)
import Time exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Material.Grid as Grid exposing (..)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Options as Options exposing (css, when)
import Util exposing (..)


-- MODEL


type alias Project =
    { title : String
    , dateCreated : String
    , timeCreated : String
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
    , dateCreated = "1/1/1990"
    , timeCreated = "12:00:00"
    , script =
        { content = "" }
    , structure = defaultStructure
    }

createProject : Time -> Project
createProject timeNow =
    { title = "New Project"
    , dateCreated = Date.fromTime timeNow |> format config config.format.date 
    , timeCreated = Date.fromTime timeNow |> format config config.format.time 
    , script =
        { content = "" }
    , structure = defaultStructure
    }

toStringDate : Date -> String
toStringDate date =
  ((toString <| Date.day date) ++ "/" ++ (toString <| Date.month date) ++ "/" ++ (toString <| Date.year date))

toStringTime : Date -> String
toStringTime date =
  ((toString <| Date.hour date) ++ ":" ++ (toString <| Date.minute date) ++ ":" ++ (toString <| Date.second date))


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
-- RENDER STRUCTURE


renderStructure : Project -> Html a
renderStructure project =
    let
        gridWidth =
            widthFromTierList project.structure.tierList
    in
        Options.div
            boxedDefault
            [ grid
                ((boxed ( 12, 0 )) ++ [ noSpacing ])
              <|
                List.append
                    (cellHeaders gridWidth project)
                    [ cellHeader gridWidth "Statistics" ]
            , grid
                ((boxed ( 12, 12 ))
                    ++ [ noSpacing
                       , Elevation.e6
                       , Color.background Color.white
                       ]
                )
                (cells gridWidth project)
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



-- RENDER DIALOG


renderDialog : Project -> Bool -> Html Msg
renderDialog project refresh =
    Options.div
        (boxedDefault |> withMaxWidth 812)
        [ Options.div
            [ Elevation.e6
            , css "height" "1024px"
            , css "position" "relative"
            , Color.background Color.white
            ]
            [ renderScript project.script refresh ]
        ]


renderScript : Script -> Bool -> Html Msg
renderScript script refresh =
    node "juicy-ace-editor"
        [ id "editor-container", on "editor-ready" (Json.Decode.succeed EditorReady) ]
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



-- ENCODE


encodeProject : Project -> Json.Encode.Value
encodeProject project =
    Json.Encode.object
        [ ( "title", Json.Encode.string project.title )
        , ( "script", encodeScript project.script )
        , ( "structure", encodeStructure project.structure )
        , ( "dateCreated", Json.Encode.string <| toString project.dateCreated )
        , ( "timeCreated", Json.Encode.string <| toString project.timeCreated)
        ]


encodeScript : Script -> Json.Encode.Value
encodeScript script =
    Json.Encode.object
        [ ( "content", Json.Encode.string script.content ) ]


encodeStructure : Structure -> Json.Encode.Value
encodeStructure structure =
    Json.Encode.object
        [ ( "tierList", Json.Encode.list (List.map encodeTier structure.tierList) ) ]


encodeTier : Tier -> Json.Encode.Value
encodeTier tier =
    Json.Encode.object
        [ ( "id", Json.Encode.string tier.id )
        , ( "name", Json.Encode.string tier.name )
        ]
