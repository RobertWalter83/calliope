module App exposing (main)

import Array exposing (..)
import Constants as Const exposing (..)
import Date exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format)
import Dict exposing (..)
import Json.Decode exposing (..)
import Json.Encode exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events as Html exposing (..)
import Material
import Material.Button as Button
import Material.Card as Card
import Material.Color as Color
import Material.Elevation as Elevation
import Material.Grid as Grid
import Material.Layout as Layout
import Material.Options as Options
import Material.Textfield as Textfield
import Ports exposing (..)
import Task exposing (..)
import Time exposing (..)
import Util exposing (..)


-- MODEL


type alias ModelMdl =
    { mdl : Material.Model
    , model : Model
    }


type alias Model =
    { viewSelected : Int
    , projectActive : Int
    , projectsAll : Dict Int Project
    , refreshEditorContent : Bool
    , raisedCard : Int
    }


type alias Project =
    { title : String
    , refreshEditor : Bool
    , titleEditable : Bool
    , dateCreated : String
    , timeCreated : String
    , lastChange : String
    , script : Script
    , tierList : List Tier
    }


type alias Script =
    { content : String
    }


type alias Scene =
    { name : String
    , location : String
    }


type alias Tier =
    { id : String
    , name : String
    }


type alias Mdl =
    Material.Model


type Msg
    = Mdl (Material.Msg Msg)
    | SelectView Int
    | Save
    | AddNewProject
    | OpenProject Int
    | AddNewProjectNow Time
    | TimeStamp Time
    | Raise Int
    | NoOp
    | UpdateEditorContent String
    | EditorReady
    | TitleEditable Int Bool
    | EditTitle Int String


type View
    = Overview
    | Structure
    | Dialog



-- MAIN


main : Program Never



--## (Maybe Model)


main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : ( ModelMdl, Cmd Msg )
init =
    ( { mdl = Material.model, model = defaultModel }, Layout.sub0 Mdl )



{--##
   init : Maybe Model -> ( ModelMdl, Cmd Msg )
   init maybeModel =
      let
           modelInit =
               transformMaybe maybeModel defaultModel (\m -> { m | refreshEditorContent = True } )
       in
           ( { mdl = Material.model, model = modelInit }, Layout.sub0 Mdl )
-}


defaultModel : Model
defaultModel =
    { viewSelected = 0
    , projectActive = -1
    , projectsAll = Dict.empty
    , refreshEditorContent = False
    , raisedCard = -1
    }


defaultTierList : List Tier
defaultTierList =
    [ { id = "Scenes", name = "Scenes" }, { id = "Scripts", name = "Scripts" } ]


rgView : Array View
rgView =
    Array.fromList [ Overview, Structure, Dialog ]



-- UPDATE


update : Msg -> ModelMdl -> ( ModelMdl, Cmd Msg )
update msg modelMdl =
    let
        modelCurrent =
            modelMdl.model
    in
        case msg of
            Mdl msg' ->
                Material.update msg' modelMdl

            Raise cardIndex ->
                raiseCard modelCurrent cardIndex |> withMdl modelMdl

            SelectView index ->
                selectView modelCurrent index |> withMdl modelMdl

            UpdateEditorContent contentNew ->
                updateProjectContent modelCurrent contentNew |> withMdl modelMdl

            EditorReady ->
                ( modelMdl, configureAce Const.aceTheme )

            TitleEditable index isEditable ->
                titleEditable modelCurrent index isEditable |> withMdl modelMdl

            EditTitle index titleNew ->
                editTitle modelCurrent index titleNew |> withMdl modelMdl

            AddNewProject ->
                ( modelMdl, cmdWithTimeNow AddNewProjectNow )

            AddNewProjectNow timeNow ->
                addNewProject modelCurrent timeNow |> withMdl modelMdl

            OpenProject index ->
                openProject modelCurrent index |> withMdlCmd modelMdl (cmdWithTimeNow TimeStamp)

            TimeStamp timeNow ->
                timeStamp modelCurrent timeNow |> withMdl modelMdl

            Save ->
                withMdl modelMdl modelMdl.model

            --##( modelMdl, encodeAppState modelMdl.model |> save )
            NoOp ->
                withMdl modelMdl modelMdl.model


updateProject : Model -> Int -> (Project -> Maybe Project) -> Model
updateProject model key mapping =
    let
        projectsAll =
            updateItemInDict model.projectsAll key mapping
    in
        { model | projectsAll = projectsAll }


cmdWithTimeNow : (Time -> Msg) -> Cmd Msg
cmdWithTimeNow msg =
    Task.perform (\_ -> NoOp) msg Time.now


timeStamp : Model -> Time -> Model
timeStamp model timeNow =
    let
        stringDate =
            Date.fromTime timeNow |> format config config.format.dateTime
    in
        updateProject model model.projectActive (\p -> Just { p | lastChange = stringDate })


raiseCard : Model -> Int -> Model
raiseCard modelCurrent cardIndex =
    { modelCurrent | raisedCard = cardIndex }


selectView : Model -> Int -> Model
selectView modelCurrent key =
    { modelCurrent | viewSelected = key, refreshEditorContent = refreshEditorContent key }


titleEditable : Model -> Int -> Bool -> Model
titleEditable model key isEditable =
    updateProject model key (\p -> Just { p | titleEditable = isEditable })


editTitle : Model -> Int -> String -> Model
editTitle model key titleNew =
    updateProject model key (\p -> Just { p | title = titleNew })


createProject : Time -> Project
createProject timeNow =
    let
        ( stringDate, stringTime, stringDateTime ) =
            ( (Date.fromTime timeNow |> format config config.format.date)
            , (Date.fromTime timeNow |> format config config.format.time)
            , (Date.fromTime timeNow |> format config config.format.dateTime)
            )
    in
        { title = Const.newProject
        , refreshEditor = False
        , titleEditable = False
        , lastChange = stringDateTime
        , dateCreated = stringDate
        , timeCreated = stringTime
        , script =
            { content = "" }
        , tierList = defaultTierList
        }


addNewProject : Model -> Time -> Model
addNewProject modelCurrent timeNow =
    let
        projectNew =
            createProject timeNow

        indexNew =
            Dict.values modelCurrent.projectsAll |> List.length

        projectsAll =
            Dict.insert indexNew projectNew modelCurrent.projectsAll
    in
        { modelCurrent | projectActive = indexNew, projectsAll = projectsAll }


openProject : Model -> Int -> Model
openProject modelCurrent index =
    { modelCurrent | projectActive = index, viewSelected = 1 }


refreshEditorContent : Int -> Bool
refreshEditorContent index =
    if ((indexToView index) == Dialog) then
        True
    else
        False


withMdl : ModelMdl -> Model -> ( ModelMdl, Cmd Msg )
withMdl modelMdl modelNew =
    withMdlCmd modelMdl Cmd.none modelNew


withMdlCmd : ModelMdl -> Cmd Msg -> Model -> ( ModelMdl, Cmd Msg )
withMdlCmd modelMdl cmd modelNew =
    ( { modelMdl | model = modelNew }, cmd )


indexToView : Int -> View
indexToView i =
    Array.get i rgView |> Maybe.withDefault Overview


updateProjectContent : Model -> String -> Model
updateProjectContent model contentNew =
    let
        model =
            updateProject model model.projectActive (\p -> Just { p | script = updateScript p.script contentNew })
    in
        { model | refreshEditorContent = False }


updateScript : Script -> String -> Script
updateScript script contentNew =
    { script | content = contentNew }



-- VIEW


view : ModelMdl -> Html Msg
view modelMdl =
    let
        layoutContent =
            if ((indexToView modelMdl.model.viewSelected) == Overview) then
                layoutOverview
            else
                layoutDefault
    in
        Layout.render Mdl
            modelMdl.mdl
            (layoutProperties modelMdl.model.viewSelected)
            (layoutContent modelMdl)


layoutProperties : Int -> List (Layout.Property Msg)
layoutProperties viewSelected =
    [ Layout.fixedHeader
    , Layout.selectedTab viewSelected
    , Layout.onSelectTab SelectView
    , if ((indexToView viewSelected) == Overview) then
        Layout.transparentHeader
      else
        Options.nop
    ]


layoutOverview : ModelMdl -> Layout.Contents Msg
layoutOverview modelMdl =
    { header = viewOverviewHeader
    , drawer = []
    , tabs = ( [], [] )
    , main = [ stylesheetOverviewHeader, viewMain modelMdl ]
    }


layoutDefault : ModelMdl -> Layout.Contents Msg
layoutDefault modelMdl =
    { header = viewDefaultHeader modelMdl
    , drawer = []
    , tabs = ( tabTitles, [ Color.background (Color.color Color.Teal Color.S600) ] )
    , main = [ viewMain modelMdl ]
    }


tabTitles : List (Html Msg)
tabTitles =
    Array.map (\v -> toString v |> text) rgView |> Array.toList


viewMain : ModelMdl -> Html Msg
viewMain modelMdl =
    let
        model =
            modelMdl.model

        viewSelected =
            indexToView model.viewSelected

        maybeProject =
            Dict.get model.projectActive model.projectsAll

        renderedContent =
            case viewSelected of
                Overview ->
                    renderOverview modelMdl

                Dialog ->
                    transformMaybe maybeProject errorView (\p -> renderDialog p model.refreshEditorContent)

                Structure ->
                    transformMaybe maybeProject errorView (\p -> renderStructure p)
    in
        Options.div [ Options.css "background" Const.pathBackground ]
            [ renderedContent
            , Button.render Mdl
                [ 1 ]
                modelMdl.mdl
                [ Button.raised
                , Button.ripple
                , Button.colored
                , Button.onClick Save
                ]
                [ text Const.saveButton ]
            ]


errorView : Html Msg
errorView =
    text Const.messageError


renderOverview : ModelMdl -> Html Msg
renderOverview modelMdl =
    let
        polaroidCreateNew =
            renderPolaroidCreateNew modelMdl Const.urlNewPolaroid Const.messageNewProject

        keys =
            Dict.keys modelMdl.model.projectsAll

        --sorted = List.sortBy .lastChange keys
        --recent = List.take 5 sorted
        polaroidsProjects =
            List.indexedMap (renderPolaroidProject modelMdl Const.urlOpenPolaroid Const.messageOpenProject)
                keys
    in
        Options.div (boxed ( 100, 20 ) |> and (Options.css "height" "1024px"))
            [ Options.div
                [ Options.css "display" "flex"
                , Options.css "flex-flow" "row wrap"
                , Options.css "align-items" "flex-start"
                , Options.css "width" "100%"
                ]
                ([ Options.div
                    [ Options.css "min-width" "300px"
                    , Options.css "max-width" "300px"
                    , Options.css "width" "300px"
                    , Options.css "padding" "12px"
                    , Options.css "border-right" "2px dashed grey"
                    , Options.css "margin-right" "44px"
                    ]
                    [ polaroidCreateNew ]
                 ]
                    ++ ([ Options.div
                            [ Options.css "padding" "12px" ]
                            polaroidsProjects
                        ]
                       )
                )
            ]


renderPolaroidCreateNew : ModelMdl -> String -> String -> Html Msg
renderPolaroidCreateNew modelMdl urlBackground userMessage =
    let
        textHead =
            Options.div [] [ text Const.newProject ]
    in
        renderPolaroid urlBackground userMessage ( modelMdl.model.raisedCard, 0 ) ( AddNewProject, NoOp ) textHead


renderPolaroidProject : ModelMdl -> String -> String -> Int -> Int -> Html Msg
renderPolaroidProject modelMdl urlBackground userMessage cardIndex keyProject =
    let
        maybeProject =
            Dict.get keyProject modelMdl.model.projectsAll

        textHead =
            renderProjectTitle modelMdl keyProject cardIndex
    in
        renderPolaroid urlBackground userMessage ( modelMdl.model.raisedCard, (cardIndex + 1) ) ( NoOp, (OpenProject keyProject) ) textHead


renderPolaroid : String -> String -> ( Int, Int ) -> ( Msg, Msg ) -> Html Msg -> Html Msg
renderPolaroid urlBackground userMessage ( raisedCardIndex, cardIndex ) ( onClickView, onClickText ) textHead =
    Card.view
        [ if raisedCardIndex == cardIndex then
            Elevation.e8
          else
            Elevation.e2
        , Elevation.transition 250
        , Options.css "width" "256px"
        , Options.attribute <| Html.onMouseEnter (Raise cardIndex)
        , Options.attribute <| Html.onMouseLeave (Raise -1)
        , Options.attribute <| Html.onClick onClickView
        , Options.css "margin" "0"
        , Options.css "padding" "12px"
        ]
        [ Card.title
            [ Options.css "background" <| urlBackground ++ "center / cover"
            , Options.css "height" "256px"
            , Options.css "padding" "0"
            ]
            [ Card.head
                [ Color.text Color.white
                , Options.scrim 0.6
                , Options.css "padding" "12px"
                , Options.css "width" "208px"
                ]
                [ textHead ]
            ]
        , Card.text
            [ Options.css "padding" "16px 0px 12px 0px"
            , Options.css "font-family" "caveat"
            , Options.css "font-weight" "700"
            , Options.css "font-size" "24px"
            , Options.css "width" "100%"
            , Options.attribute <| Html.onClick onClickText
            , Color.text Color.black
            ]
            [ text userMessage ]
        ]


viewOverviewHeader : List (Html Msg)
viewOverviewHeader =
    [ Layout.row
        [ Options.css "height" "320px"
        , Options.css "min-height" "320px"
        , Options.css "max-height" "320px"
        , Options.css "transition" "height 333ms ease-in-out 0s"
        , Options.css "padding" "24px"
        ]
        [ Options.div
            [ Color.text Color.black
            , Options.css "font-size" "24px"
            , Options.css "padding-bottom" "200px"
            ]
            [ text Const.messageWelcome ]
        ]
    ]


viewDefaultHeader : ModelMdl -> List (Html Msg)
viewDefaultHeader modelMdl =
    [ Layout.row [ Options.css "transition" "height 333ms ease-in-out 0s" ]
        [ renderProjectTitle modelMdl modelMdl.model.projectActive 0 ]
    ]


renderProjectTitle : ModelMdl -> Int -> Int -> Html Msg
renderProjectTitle modelMdl keyProject indexTextfield =
    let
        maybeProject =
            Dict.get keyProject modelMdl.model.projectsAll

        ( title, titleEditable ) =
            transformMaybe maybeProject ( Const.newProject, False ) (\p -> ( p.title, p.titleEditable ))
    in
        if (titleEditable) then
            Textfield.render Mdl
                [ indexTextfield ]
                modelMdl.mdl
                [ Textfield.text'
                , Textfield.onInput <| EditTitle keyProject
                , Textfield.onBlur <| TitleEditable keyProject False
                , Textfield.value title
                , Options.css "font-size" "24px"
                ]
        else
            Options.div [ Options.attribute <| Html.onClick (TitleEditable keyProject True) ] [ text title ]



-- RENDER STRUCTURE


renderStructure : Project -> Html a
renderStructure project =
    let
        gridWidth =
            widthFromTierList project.tierList
    in
        Options.div
            boxedDefault
            [ Grid.grid
                ((boxed ( 12, 0 )) ++ [ Grid.noSpacing ])
              <|
                List.append
                    (cellHeaders gridWidth project)
                    [ cellHeader gridWidth "Statistics" ]
            , Grid.grid
                ((boxed ( 12, 12 ))
                    ++ [ Grid.noSpacing
                       , Elevation.e6
                       , Color.background Color.white
                       ]
                )
                (cells gridWidth project)
            ]


cellHeaders : Int -> Project -> List (Grid.Cell a)
cellHeaders gridWidth project =
    List.map (cellHeaderFromTier gridWidth) project.tierList


cellHeaderFromTier : Int -> Tier -> Grid.Cell a
cellHeaderFromTier gridWidth tier =
    cellHeader gridWidth tier.name


cellHeader : Int -> String -> Grid.Cell a
cellHeader gridWidth stHeader =
    Grid.cell
        [ Grid.size Grid.All gridWidth ]
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


cells : Int -> Project -> List (Grid.Cell a)
cells gridWidth project =
    List.map (cellFromTier gridWidth) project.tierList


cellFromTier : Int -> Tier -> Grid.Cell a
cellFromTier gridWidth tier =
    Grid.cell
        [ Grid.size Grid.All gridWidth
        , Options.css "height" "200px"
        ]
        [ text <| "Description of " ++ tier.name ++ " goes here!." ]



-- RENDER DIALOG


renderDialog : Project -> Bool -> Html Msg
renderDialog project refresh =
    Options.div
        (boxedDefault |> withMaxWidth 812)
        [ Options.div
            [ Elevation.e6
            , Options.css "height" "1024px"
            , Options.css "position" "relative"
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



-- STYLING


stylesheetOverviewHeader : Html a
stylesheetOverviewHeader =
    Options.stylesheet """\x0D
  .mdl-layout__header--transparent {\x0D
    background: url('assets/march.jpg') 0 45% no-repeat;\x0D
    background-size: 100% auto\x0D
  }\x0D
"""



-- SUBS


subscriptions : ModelMdl -> Sub Msg
subscriptions modelMdl =
    Sub.batch
        [ updateEditorContent UpdateEditorContent ]



-- ENCODE
{-
   encodeAppState : Model -> Json.Encode.Value
   encodeAppState model =
       Json.Encode.object
           [ ( "viewSelected", Json.Encode.int model.viewSelected )
           , ( "projectActive", encodeMaybeProject model.projectActive )
           , ( "projectsAll", Json.Encode.list (List.map encodeProject model.projectsAll) )
           , ( "refreshEditorContent", Json.Encode.bool model.refreshEditorContent )
           , ( "raisedCard", Json.Encode.int model.raisedCard )
           ]


   encodeMaybeProject : Maybe Project -> Json.Encode.Value
   encodeMaybeProject projectInput =
       transformMaybe projectInput Json.Encode.null (\p -> encodeProject p)


   encodeProject : Project -> Json.Encode.Value
   encodeProject project =
       Json.Encode.object
           [ ( "title", Json.Encode.string project.title )
           , ( "script", encodeScript project.script )
           , ( "tierList", Json.Encode.list (List.map encodeTier project.tierList) )
           , ( "dateCreated", Json.Encode.string project.dateCreated )
           , ( "timeCreated", Json.Encode.string project.timeCreated )
           , ( "lastChange", Json.Encode.string project.lastChange )
           , ( "refreshEditor", Json.Encode.bool project.refreshEditor )
           , ( "titleEditable", Json.Encode.bool project.titleEditable )
           ]


   encodeScript : Script -> Json.Encode.Value
   encodeScript script =
       Json.Encode.object
           [ ( "content", Json.Encode.string script.content ) ]


   encodeTier : Tier -> Json.Encode.Value
   encodeTier tier =
       Json.Encode.object
           [ ( "id", Json.Encode.string tier.id )
           , ( "name", Json.Encode.string tier.name )
           ]
-}
