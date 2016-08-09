module Project
    exposing
        ( Project
        , defaultProject
        , createProject
        , defaultStructure
        , renderDialog
        , renderStructure
        , encodeProject
        , renderProjectTitle
        , update
        , subscriptions 
        , Msg
        )

import Ports exposing(..)
import Date exposing (..)
import Time exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Html exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Material
import Material.Grid as Grid exposing (..)
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Material.Options as Options exposing (css, when)
import Util exposing (..)


-- MODEL



-- DEFAULTS




-- UPDATE


update : Msg -> Project -> ( Project, Cmd Msg )
update msg project =
    case msg of
        Mdl msg' ->
            Material.update msg' project 

        UpdateEditorContent contentNew -> let data = project.projectData in
            ({ project | projectData = { data | script = updateScript data.script (Debug.log "contentNew" contentNew), refreshEditor = False }}, Cmd.none)

        EditorReady ->
            ( project, Cmd.none )

        TitleEditable isEditable -> let data = project.projectData in
            ( { project | projectData = { data | titleEditable = isEditable } }, Cmd.none )

        EditTitle titleNew -> let data = project.projectData in
            ( { project | projectData = { data | title = titleNew } }, Cmd.none )






-- RENDERING
-- RENDER STRUCTURE


renderStructure : Project -> Html a
renderStructure project =
    let
        gridWidth =
            widthFromTierList project.projectData.structure.tierList
    in
        Options.div
            boxedDefault
            [ grid
                ((boxed ( 12, 0 )) ++ [ noSpacing ])
              <|
                List.append
                    (cellHeaders gridWidth project.projectData)
                    [ cellHeader gridWidth "Statistics" ]
            , grid
                ((boxed ( 12, 12 ))
                    ++ [ noSpacing
                       , Elevation.e6
                       , Color.background Color.white
                       ]
                )
                (cells gridWidth project.projectData)
            ]


cellHeaders : Int -> ProjectData -> List (Cell a)
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


cells : Int -> ProjectData -> List (Cell a)
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
    let doRefresh = Debug.log "refresh:" (refresh && project.projectData.refreshEditor) 
    in
    Options.div
        (boxedDefault |> withMaxWidth 812)
        [ Options.div
            [ Elevation.e6
            , css "height" "1024px"
            , css "position" "relative"
            , Color.background Color.white
            ]
            [ renderScript project.projectData.script doRefresh ]
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



-- RENDER PROJECT TITLE
--renderProjectTitle : Project -> (Parts.Msg Material.Model App.Msg -> App.Msg) -> Material.Model -> Html App.Msg


renderProjectTitle project =
    let data = project.projectData in
    if data.titleEditable then
        Textfield.render Mdl
            [ 13 ]
            Material.model
            [ Textfield.text'
            , Textfield.onInput EditTitle
            , Textfield.onBlur <| TitleEditable False
            , Textfield.value data.title
            , css "font-size" "24px"
            ]
    else
        Options.div [ Options.attribute <| Html.onClick (TitleEditable True) ] [ text data.title ]


-- ENCODE


encodeProject : Project -> Json.Encode.Value
encodeProject project = let data = project.projectData in
    Json.Encode.object
        [ ( "title", Json.Encode.string data.title )
        , ( "script", encodeScript data.script )
        , ( "structure", encodeStructure data.structure )
        , ( "dateCreated", Json.Encode.string data.dateCreated )
        , ( "timeCreated", Json.Encode.string data.timeCreated )
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


-- SUBSCRIPTIONS

subscriptions : Project -> Sub Msg
subscriptions project =
    updateEditorContent UpdateEditorContent