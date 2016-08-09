module App exposing (..)

import Ports exposing (..)
import Task exposing (..)
import Date exposing (..)
import Time exposing (..)
import Date.Extra.Config.Config_en_us exposing (config)
import Date.Extra.Format as Format exposing (format)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Html exposing (..)
import Json.Encode exposing (..)
import Json.Decode exposing (..)
import Html.App as App
import Json.Encode exposing (..)
import Array exposing (..)
import Material
import Material.Card as Card
import Material.Color as Color
import Material.Grid as Grid exposing (..)
import Material.Layout as Layout
import Material.Options as Options exposing (css, when)
import Material.Button as Button
import Material.Elevation as Elevation
import Material.Textfield as Textfield
import Util exposing (..)


-- MODEL


type alias ModelMdl =
    { mdl : Material.Model
    , model : Model
    }


type alias Model =
    { viewSelected : Int
    , projectActive : Project
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


type alias PolaroidParams =
    { index : Int, modelMdl : ModelMdl, onClick : Msg, pathBackground : String, messageTuple : ( String, String ) }


type alias Mdl =
    Material.Model


type Msg
    = Mdl (Material.Msg Msg)
    | SelectView Int
    | Save
    | CreateNewProject
    | OpenProject Project
    | CreateNewProjectNow Time
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
            case maybeModel of
                Nothing ->
                    addNewProject defaultProject

                Maybe.Just model ->
                    { model | refreshEditorContent = True }
    in
        ( { mdl = Material.model, model = modelInit }, Layout.sub0 Mdl )


defaultProject : Project
defaultProject =
    createProjectAt "01/Jan/1990" "12:00"


createProject : Time -> Project
createProject timeNow =
    createProjectAt
        (Date.fromTime timeNow |> format config config.format.date)
        (Date.fromTime timeNow |> format config config.format.time)


createProjectAt : String -> String -> Project
createProjectAt date time =
    { title = "New Project"
    , refreshEditor = False
    , titleEditable = False
    , dateCreated = date
    , timeCreated = time
    , script =
        { content = "" }
    , tierList = defaultTierList
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
                ( modelMdl, configureAce "ace/theme/textmate" )

            TitleEditable projectMsg ->
                withMdl modelMdl modelMdl.model

            EditTitle titleNew ->
                editTitle modelCurrent titleNew |> withMdl modelMdl

            CreateNewProject ->
                ( modelMdl, cmdTimeNow )

            CreateNewProjectNow timeNow ->
                createNewProject modelCurrent timeNow |> withMdl modelMdl

            OpenProject project ->
                openProject modelCurrent project |> withMdl modelMdl

            Save ->
                ( modelMdl, encodeAppState modelMdl.model |> save )

            NoOp ->
                withMdl modelMdl modelMdl.model


cmdTimeNow : Cmd Msg
cmdTimeNow =
    Task.perform (\_ -> NoOp) CreateNewProjectNow Time.now


addNewProject : Project -> Model
addNewProject project =
    { viewSelected = 0
    , projectActive = project
    , projectsAll = []
    , projectsRecent = []
    , refreshEditorContent = False
    , raisedCard = -1
    }


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

        projectsRecent =
            newProject
                :: (Debug.log "projectsRecent" filter modelCurrent.projectsRecent (Debug.log "projectActive" modelCurrent.projectActive))

        projectsAll =
            newProject
                :: filter modelCurrent.projectsAll modelCurrent.projectActive
    in
        { modelCurrent | projectActive = newProject, projectsRecent = projectsRecent, projectsAll = projectsAll }


filter : List a -> a -> List a
filter list item =
    List.filter (\i -> i /= item) list


createNewProject : Model -> Time -> Model
createNewProject modelCurrent timeNow =
    let
        newProject =
            createProject timeNow

        projectsRecent =
            newProject :: modelCurrent.projectsRecent

        projectsAll =
            sortByTitle <| newProject :: modelCurrent.projectsAll

        modelNew =
            addNewProject newProject
    in
        { modelNew | projectsRecent = projectsRecent, projectsAll = projectsAll }


openProject : Model -> Project -> Model
openProject modelCurrent projectToOpen =
    let
        projectsRecent =
            updateProjectList modelCurrent.projectsRecent projectToOpen

        projectsAll =
            sortByTitle <| updateProjectList modelCurrent.projectsAll projectToOpen
    in
        { modelCurrent | projectActive = projectToOpen, projectsRecent = projectsRecent, projectsAll = projectsAll, viewSelected = 1 }


updateProjectTitle : Project -> String -> Project
updateProjectTitle projectOld titleNew =
    { projectOld | title = titleNew }


sortByTitle : List Project -> List Project
sortByTitle input =
    List.sortBy (\p -> p.title) input


updateProjectList : List Project -> Project -> List Project
updateProjectList projectsCurrent projectNew =
    let
        filtered =
            filter projectsCurrent projectNew
    in
        projectNew :: filtered


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
        projectCurrent =
            model.projectActive
    in
        { model | projectActive = { projectCurrent | script = updateScript projectCurrent.script (Debug.log "contentNew" contentNew) } }


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
    , main = [ stylesheet, viewMain modelMdl ]
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
                    renderDialog model.projectActive model.refreshEditorContent

                Structure ->
                    renderStructure model.projectActive
    in
        Options.div [ css "background" "url('assets/bg.png')" ]
            [ renderedContent
            , Button.render Mdl
                [ 1 ]
                modelMdl.mdl
                [ Button.raised
                , Button.ripple
                , Button.colored
                , Button.onClick Save
                ]
                [ text "Save" ]
            ]


renderOverview : ModelMdl -> Html Msg
renderOverview modelMdl =
    let
        lengthRecentProjects =
            List.length modelMdl.model.projectsRecent
    in
        Options.div ((boxed ( 100, 20 )) ++ [ css "height" "1024px" ])
            [ Options.div
                [ css "display" "flex"
                , css "flex-flow" "row wrap"
                  -- , css "justify-content" "space-between"
                , css "align-items" "flex-start"
                , css "width" "100%"
                ]
                ([ Options.div
                    [ css "min-width" "300px"
                    , css "max-width" "300px"
                    , css "width" "300px"
                    , css "padding" "12px"
                    , css "border-right" "2px dashed grey"
                    , css "margin-right" "44px"
                    ]
                    [ renderPolaroid modelMdl.model.projectActive <|
                        createParams 0 modelMdl CreateNewProject "assets/new.jpg" ( "New Project", "Create a brand new project" )
                    ]
                 ]
                    ++ (List.map2 renderProjectLink
                            modelMdl.model.projectsRecent
                        <|
                            List.map5 createParams
                                [1..lengthRecentProjects]
                                (List.repeat lengthRecentProjects modelMdl)
                                (List.map (\p -> OpenProject p) modelMdl.model.projectsRecent)
                                (List.repeat lengthRecentProjects "assets/existing.jpg")
                                (List.map (\p -> ( p.title, "Click here to open." )) modelMdl.model.projectsRecent)
                       )
                )
            ]


createParams : Int -> ModelMdl -> Msg -> String -> ( String, String ) -> PolaroidParams
createParams index modelMdl msg pathBackground messageTuple =
    { index = index
    , modelMdl = modelMdl
    , onClick = msg
    , pathBackground = pathBackground
    , messageTuple = messageTuple
    }


renderProjectLink : Project -> PolaroidParams -> Html Msg
renderProjectLink project params =
    Options.div
        [ css "padding" "12px" ]
        [ renderPolaroid project params ]


renderPolaroid : Project -> PolaroidParams -> Html Msg
renderPolaroid project params =
    Card.view
        [ if params.modelMdl.model.raisedCard == params.index then
            Elevation.e8
          else
            Elevation.e2
        , Elevation.transition 250
        , css "width" "256px"
        , Options.attribute <| Html.onMouseEnter (Raise params.index)
        , Options.attribute <| Html.onMouseLeave (Raise -1)
        , Options.attribute <| Html.onClick params.onClick
        , css "margin" "0"
        , css "padding" "12px"
        ]
        [ Card.title
            [ css "background" <| "url('" ++ params.pathBackground ++ "') center / cover"
            , css "height" "256px"
            , css "padding" "0"
            ]
            [ Card.head
                [ Color.text Color.white
                , Options.scrim 0.6
                , css "padding" "12px"
                , css "width" "208px"
                ]
                []
            ]
        , Card.text
            [ css "padding" "16px 0px 12px 0px"
            , css "font-family" "caveat"
            , css "font-weight" "700"
            , css "font-size" "24px"
            , css "width" "100%"
            , Color.text Color.black
            ]
            [ text <| snd params.messageTuple ]
        ]


viewOverviewHeader : List (Html Msg)
viewOverviewHeader =
    [ Layout.row
        [ css "height" "320px"
        , css "min-height" "320px"
        , css "max-height" "320px"
        , css "transition" "height 333ms ease-in-out 0s"
        , css "padding" "24px"
        ]
        [ Options.div
            [ css "display" "flex"
            , css "flex-flow" "row wrap"
            , css "justify-content" "space-between"
            , css "align-items" "flex-start"
            , css "width" "100%"
            , Color.text Color.black
            , css "font-size" "24px"
            , css "padding-bottom" "200px"
            ]
            [ text "Welcome back to Calliope!" ]
        ]
    ]


viewDefaultHeader : ModelMdl -> List (Html Msg)
viewDefaultHeader modelMdl =
    [ Layout.row [ css "transition" "height 333ms ease-in-out 0s" ]
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
            [ grid
                ((boxed ( 12, 0 )) ++ [ Grid.noSpacing ])
              <|
                List.append
                    (cellHeaders gridWidth project)
                    [ cellHeader gridWidth "Statistics" ]
            , grid
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


cells : Int -> Project -> List (Grid.Cell a)
cells gridWidth project =
    List.map (cellFromTier gridWidth) project.tierList


cellFromTier : Int -> Tier -> Grid.Cell a
cellFromTier gridWidth tier =
    cell
        [ Grid.size All gridWidth
        , css "height" "200px"
        ]
        [ text <| "Description of " ++ tier.name ++ " goes here!." ]



-- RENDER DIALOG


renderDialog : Project -> Bool -> Html Msg
renderDialog project refresh =
    let
        doRefresh =
            Debug.log "refresh:" (refresh && project.refreshEditor)
    in
        Options.div
            (boxedDefault |> withMaxWidth 812)
            [ Options.div
                [ Elevation.e6
                , css "height" "1024px"
                , css "position" "relative"
                , Color.background Color.white
                ]
                [ renderScript project.script doRefresh ]
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
    if project.titleEditable then
        Textfield.render Mdl
            [ 13 ]
            Material.model
            [ Textfield.text'
            , Textfield.onInput EditTitle
            , Textfield.onBlur <| TitleEditable False
            , Textfield.value project.title
            , css "font-size" "24px"
            ]
    else
        Options.div [ Options.attribute <| Html.onClick (TitleEditable True) ] [ text project.title ]



-- ENCODE


encodeProject : Project -> Json.Encode.Value
encodeProject project =
    Json.Encode.object
        [ ( "title", Json.Encode.string project.title )
        , ( "script", encodeScript project.script )
        , ( "tierList", Json.Encode.list (List.map encodeTier project.tierList) )
        , ( "dateCreated", Json.Encode.string project.dateCreated )
        , ( "timeCreated", Json.Encode.string project.timeCreated )
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



-- STYLING


stylesheet : Html a
stylesheet =
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


encodeAppState : Model -> Json.Encode.Value
encodeAppState model =
    Json.Encode.object
        [ ( "viewSelected", Json.Encode.int model.viewSelected )
        , ( "projectActive", encodeProject model.projectActive )
        , ( "projectsRecent", Json.Encode.list (List.map encodeProject model.projectsRecent) )
        , ( "projectsAll", Json.Encode.list (List.map encodeProject model.projectsAll) )
        , ( "refreshEditorContent", Json.Encode.bool model.refreshEditorContent )
        , ( "raisedCard", Json.Encode.int model.raisedCard )
        ]
