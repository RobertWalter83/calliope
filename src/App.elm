module App exposing (main)

import Array exposing (..)
import Constants as Const exposing(..)
import Date exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format)
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
    , projectActive : Maybe Project
    , projectsAll : List Project
    , projectsRecent : List Project
    , refreshEditorContent : Bool
    , raisedCard : Int
    }


type alias Project =
    { title : String
    , refreshEditor : Bool
    , titleEditable : Bool
    , dateCreated : String
    , timeCreated : String
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
    | OpenProject Project
    | AddNewProjectNow Time
    | Raise Int
    | NoOp
    | UpdateEditorContent String
    | EditorReady
    | TitleEditable Bool
    | EditTitle String


type View
    = Overview
    | Structure
    | Dialog



-- MAIN


main : Program (Maybe Model)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- INIT


init : Maybe Model -> ( ModelMdl, Cmd Msg )
init maybeModel =
    let
        modelInit =
            transformMaybe maybeModel defaultModel (\m -> { m | refreshEditorContent = True } )
    in
        ( { mdl = Material.model, model = modelInit }, Layout.sub0 Mdl )


defaultModel : Model
defaultModel =
    { viewSelected = 0
    , projectActive = Nothing
    , projectsAll = []
    , projectsRecent = []
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

            TitleEditable projectMsg ->
                withMdl modelMdl modelMdl.model

            EditTitle titleNew ->
                editTitle modelCurrent titleNew |> withMdl modelMdl

            AddNewProject ->
                ( modelMdl, cmdTimeNow )

            AddNewProjectNow timeNow ->
                addNewProject modelCurrent timeNow |> withMdl modelMdl

            OpenProject project ->
                openProject modelCurrent project |> withMdl modelMdl

            Save ->
                ( modelMdl, encodeAppState modelMdl.model |> save )

            NoOp ->
                withMdl modelMdl modelMdl.model


cmdTimeNow : Cmd Msg
cmdTimeNow =
    Task.perform (\_ -> NoOp) AddNewProjectNow Time.now


raiseCard : Model -> Int -> Model
raiseCard modelCurrent cardIndex =
    { modelCurrent | raisedCard = cardIndex }


selectView : Model -> Int -> Model
selectView modelCurrent index =
    { modelCurrent | viewSelected = index, refreshEditorContent = refreshEditorContent index }


editTitle : Model -> String -> Model
editTitle modelCurrent titleNew =
    let
        newProject =
            updateProjectTitle modelCurrent.projectActive titleNew

        ( filteredRecent, filteredAll ) =
            transformMaybe 
                modelCurrent.projectActive 
                ( modelCurrent.projectsRecent, modelCurrent.projectsAll ) 
                ( \p -> ( Util.filter modelCurrent.projectsRecent p, Util.filter modelCurrent.projectsAll p ) )
            

        ( projectsRecent, projectsAll ) =
            transformMaybe 
                newProject
                ( modelCurrent.projectsRecent, modelCurrent.projectsAll )
                ( \p -> (p :: filteredRecent, p :: filteredAll ))
    in
        { modelCurrent | projectActive = newProject, projectsRecent = projectsRecent, projectsAll = projectsAll }


createProject : Time -> Project
createProject timeNow =
    let
        ( stringTime, stringDate ) =
            ( (Date.fromTime timeNow |> format config config.format.date)
            , (Date.fromTime timeNow |> format config config.format.time)
            )
    in
        { title = Const.newProject
        , refreshEditor = False
        , titleEditable = False
        , dateCreated = stringTime
        , timeCreated = stringDate
        , script =
            { content = "" }
        , tierList = defaultTierList
        }


modelWithProject : Project -> Model
modelWithProject project =
    { viewSelected = 0
    , projectActive = Just project
    , projectsAll = []
    , projectsRecent = []
    , refreshEditorContent = False
    , raisedCard = -1
    }


addNewProject : Model -> Time -> Model
addNewProject modelCurrent timeNow =
    let
        newProject =
            createProject timeNow

        projectsRecent =
            newProject :: modelCurrent.projectsRecent

        projectsAll =
            sortByTitle <| newProject :: modelCurrent.projectsAll
    in
        { modelCurrent | projectActive = Just newProject, projectsRecent = projectsRecent, projectsAll = projectsAll }


openProject : Model -> Project -> Model
openProject modelCurrent projectToOpen =
    let
        projectsRecent =
            moveToHead projectToOpen modelCurrent.projectsRecent

        projectsAll =
            sortByTitle <| moveToHead projectToOpen modelCurrent.projectsAll
    in
        { modelCurrent | projectActive = Just projectToOpen, projectsRecent = projectsRecent, projectsAll = projectsAll, viewSelected = 1 }


updateProjectTitle : Maybe Project -> String -> Maybe Project
updateProjectTitle projectOld titleNew =
    transformMaybe projectOld Nothing (\p -> Just { p | title = titleNew })


sortByTitle : List Project -> List Project
sortByTitle input =
    List.sortBy (\p -> p.title) input


moveToHead : Project -> List Project -> List Project
moveToHead projectToMove projectsCurrent =
    let
        filtered =
            Util.filter projectsCurrent projectToMove
    in
        projectToMove :: filtered


refreshEditorContent : Int -> Bool
refreshEditorContent index =
    if ((indexToView index) == Dialog) then
        True
    else
        False


withMdl : ModelMdl -> Model -> ( ModelMdl, Cmd Msg )
withMdl modelMdl modelNew =
    ( { modelMdl | model = modelNew }, Cmd.none )


indexToView : Int -> View
indexToView i =
    Array.get i rgView |> Maybe.withDefault Overview


updateProjectContent : Model -> String -> Model
updateProjectContent model contentNew =
    let
        projectActive = transformMaybe model.projectActive Nothing (\p -> Just { p | script = updateScript p.script contentNew })
    in
        { model | projectActive = projectActive, refreshEditorContent = False }


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

        renderedContent =
            case viewSelected of
                Overview ->
                    renderOverview modelMdl

                Dialog ->
                    transformMaybe model.projectActive errorView ( \p -> renderDialog p model.refreshEditorContent )

                Structure ->
                    transformMaybe model.projectActive errorView ( \p -> renderStructure p )
                            
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
        projectsRecent =
            modelMdl.model.projectsRecent

        lengthProjectsRecent =
            List.length projectsRecent 

        polaroidCreateNew =
            renderPolaroid modelMdl Const.urlNewPolaroid Const.messageNewProject Nothing 0

        polaroidsProjectsRecent =
            List.map2 (renderProjectLink modelMdl Const.urlOpenPolaroid Const.messageOpenProject )
                projectsRecent
                [1..lengthProjectsRecent] 
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
                    ++ polaroidsProjectsRecent
                )
            ]


renderProjectLink : ModelMdl -> String -> String -> Project -> Int -> Html Msg
renderProjectLink modelMdl urlBackground userMessage project cardIndex =
    Options.div
        [ Options.css "padding" "12px" ]
        [ renderPolaroid modelMdl urlBackground userMessage (Just project) cardIndex ]


renderPolaroid : ModelMdl -> String -> String -> Maybe Project -> Int -> Html Msg
renderPolaroid modelMdl urlBackground userMessage maybeProject cardIndex =
    let
        ( onClick, title ) =
            transformMaybe maybeProject ( AddNewProject, Const.newProject ) (\p -> ( OpenProject p, p.title ) )
    in
        Card.view
            [ if modelMdl.model.raisedCard == cardIndex then
                Elevation.e8
              else
                Elevation.e2
            , Elevation.transition 250
            , Options.css "width" "256px"
            , Options.attribute <| Html.onMouseEnter (Raise cardIndex)
            , Options.attribute <| Html.onMouseLeave (Raise -1)
            , Options.attribute <| Html.onClick onClick
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
                    []
                ]
            , Card.text
                [ Options.css "padding" "16px 0px 12px 0px"
                , Options.css "font-family" "caveat"
                , Options.css "font-weight" "700"
                , Options.css "font-size" "24px"
                , Options.css "width" "100%"
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
        [ renderProjectTitle modelMdl.model.projectActive ]
    ]



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



-- RENDER PROJECT TITLE
--renderProjectTitle : Project -> (Parts.Msg Material.Model App.Msg -> App.Msg) -> Material.Model -> Html App.Msg


renderProjectTitle project =
    transformMaybe project errorView
        (\p -> 
            if p.titleEditable then
                Textfield.render Mdl
                    [ 13 ]
                    Material.model
                    [ Textfield.text'
                    , Textfield.onInput EditTitle
                    , Textfield.onBlur <| TitleEditable False
                    , Textfield.value p.title
                    , Options.css "font-size" "24px"
                    ]
            else
                Options.div [ Options.attribute <| Html.onClick (TitleEditable True) ] [ text p.title ]
        )



-- STYLING


stylesheetOverviewHeader : Html a
stylesheetOverviewHeader =
    Options.stylesheet """
  .mdl-layout__header--transparent {
    background: url('assets/march.jpg') 0 45% no-repeat;
    background-size: 100% auto
  }
"""



-- SUBS


subscriptions : ModelMdl -> Sub Msg
subscriptions modelMdl =
    Sub.batch
        [ updateEditorContent UpdateEditorContent ]



-- ENCODE


encodeAppState : Model -> Json.Encode.Value
encodeAppState model =
    Json.Encode.object
        [ ( "viewSelected", Json.Encode.int model.viewSelected )
        , ( "projectActive", encodeMaybeProject model.projectActive )
        , ( "projectsRecent", Json.Encode.list (List.map encodeProject model.projectsRecent) )
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
