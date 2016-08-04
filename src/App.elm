port module App exposing (..)

import Calliope exposing (..)
import Task exposing (..)
import Date exposing (..)
import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events as Html exposing (..)
import Html.App as App
import Json.Encode exposing (..)
import Array exposing (..)
import Material
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Grid as Grid exposing (..)
import Material.Options as Options exposing (css, when)
import Material.Button as Button exposing (..)
import Material.Elevation as Elevation
import Util exposing (..)


-- MODEL


type alias AppState =
    { viewSelected : Int
    , projectActive : Project
    , projectsAll : List Project
    , projectsRecent : List Project
    , titleEditable : Bool
    , refreshEditorContent : Bool
    }


type alias AppWithMdl =
    { mdl : Material.Model
    , appState : AppState
    }


type Msg
    = Mdl Material.Msg
    | SelectView Int
    | ToggleEditableTitle 
    | EditTitle String
    | GetEditorContent String
    | EditorReady Calliope.Msg
    | Save
    | CreateNewProject
    | OpenProject Project
    | CreateNewProjectNow Date 
    | NoOp

cmdDateNow : Cmd Msg
cmdDateNow = Task.perform (\_ -> NoOp) CreateNewProjectNow Date.now

type View
    = Welcome
    | Structure
    | Dialog



-- PORTS


port configureAce : String -> Cmd m
 

port save : Json.Encode.Value -> Cmd m


port getEditorContent : (String -> msg) -> Sub msg



-- MAIN


main : Program (Maybe AppState)
main =
    App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Maybe AppState -> ( AppWithMdl, Cmd Msg )
init maybeAppState =
    let
        appStateInit =
            case maybeAppState of
                Nothing ->
                    wrapWithMdl defaultAppState

                Maybe.Just appState ->
                    wrapWithMdl { appState | refreshEditorContent = True }
    in
        ( appStateInit, Layout.sub0 Mdl )


wrapWithMdl : AppState -> AppWithMdl
wrapWithMdl appState =
    { mdl = Material.model
    , appState = appState
    } 

defaultAppState : AppState
defaultAppState =
    { viewSelected = 0 
    , projectActive = Calliope.defaultProject
    , projectsAll = []
    , projectsRecent = []
    , titleEditable = False
    , refreshEditorContent = False
    }


createProjectWithDate : Date -> AppState
createProjectWithDate dateNow =
    { viewSelected = 0
    , projectActive = Calliope.createProject dateNow 
    , projectsAll = []
    , projectsRecent = []
    , titleEditable = False
    , refreshEditorContent = False
    }



-- UPDATE


update : Msg -> AppWithMdl -> ( AppWithMdl, Cmd Msg )
update msg appWithMdl =
    let
        appStateCurrent =
            appWithMdl.appState
    in
        case msg of
            Mdl msg ->
                Material.update Mdl msg appWithMdl

            SelectView index ->
                wrapWithCmdNone { appWithMdl | appState = selectView appStateCurrent index }

            ToggleEditableTitle ->
                wrapWithCmdNone { appWithMdl | appState = toggleEditableTitle appStateCurrent }

            EditTitle titleNew ->
                wrapWithCmdNone { appWithMdl | appState = editTitle appStateCurrent titleNew }

            EditorReady subMsg ->
                ( appWithMdl, configureAce "ace/theme/textmate" )

            GetEditorContent content ->
                wrapWithCmdNone { appWithMdl | appState = setEditorContent appStateCurrent content }

            Save ->
                ( appWithMdl
                , encodeAppState appWithMdl.appState
                    |> save
                )

            CreateNewProjectNow dateNow -> 
                wrapWithCmdNone { appWithMdl | appState = createNewProject appStateCurrent dateNow }

            CreateNewProject ->
                ( appWithMdl, cmdDateNow )
                
            OpenProject project ->
                wrapWithCmdNone { appWithMdl | appState = openProject appStateCurrent project }

            NoOp -> 
                wrapWithCmdNone appWithMdl


selectView : AppState -> Int -> AppState
selectView appStateCurrent index =
    { appStateCurrent | viewSelected = index, refreshEditorContent = refreshEditorContent index }


toggleEditableTitle : AppState -> AppState
toggleEditableTitle appStateCurrent =
    { appStateCurrent | titleEditable = (not appStateCurrent.titleEditable) }


editTitle : AppState -> String -> AppState
editTitle appStateCurrent titleNew =
    { appStateCurrent | projectActive = updateProjectTitle appStateCurrent.projectActive titleNew }


setEditorContent : AppState -> String -> AppState
setEditorContent appStateCurrent content =
    { appStateCurrent | projectActive = Calliope.updateProject appStateCurrent.projectActive content, refreshEditorContent = False }


createNewProject : AppState -> Date -> AppState
createNewProject appStateCurrent dateNow =
    let
        -- TODO handle name clashes
        projectsRecent =
            [ appStateCurrent.projectActive ] ++ appStateCurrent.projectsRecent

        projectsAll =
            [ appStateCurrent.projectActive ] ++ appStateCurrent.projectsAll

        defaultAppStateI = createProjectWithDate dateNow
    in
        { defaultAppStateI | projectsRecent = projectsRecent, projectsAll = projectsAll }


openProject : AppState -> Project -> AppState
openProject appStateCurrent projectToOpen =
    let
        projectsRecent =
            updateProjectList appStateCurrent.projectsRecent projectToOpen

        projectsAll =
            List.sortBy .title <| updateProjectList appStateCurrent.projectsAll projectToOpen
    in
        { appStateCurrent | projectActive = projectToOpen, projectsRecent = projectsRecent, projectsAll = projectsAll }


updateProjectList : List Project -> Project -> List Project
updateProjectList projectsCurrent projectNew =
    let
        filtered =
            List.filter (\p -> (/=) p projectNew) projectsCurrent
    in
        [ projectNew ] ++ filtered


refreshEditorContent : Int -> Bool
refreshEditorContent index =
    if ((indexToView index) == Dialog) then
        True
    else
        False


wrapWithCmdNone : AppWithMdl -> ( AppWithMdl, Cmd Msg )
wrapWithCmdNone appWithMdl =
    ( appWithMdl, Cmd.none )


indexToView : Int -> View
indexToView i =
    Array.get i rgView |> Maybe.withDefault Welcome


updateProjectTitle : Calliope.Project -> String -> Calliope.Project
updateProjectTitle projectOld titleNew =
    { projectOld | title = titleNew }



-- VIEW


view : AppWithMdl -> Html Msg
view appWithMdl =
    let
        layoutContent =
            if ((indexToView appWithMdl.appState.viewSelected) == Welcome) then
                layoutWelcome
            else
                layoutDefault
    in
        Layout.render Mdl
            appWithMdl.mdl
            (layoutProperties appWithMdl.appState.viewSelected)
            (layoutContent appWithMdl)


layoutProperties : Int -> List (Layout.Property Msg)
layoutProperties viewSelected =
    [ Layout.fixedHeader
    , Layout.selectedTab viewSelected
    , Layout.onSelectTab SelectView
    , if ((indexToView viewSelected) == Welcome) then
        Layout.transparentHeader
      else
        Options.nop
    ]


layoutWelcome : AppWithMdl -> Layout.Contents Msg
layoutWelcome appWithMdl =
    { header = viewWelcomeHeader
    , drawer = []
    , tabs = ( tabTitles, [ Color.background (Color.color Color.BlueGrey Color.S400) ] )
    , main = [ stylesheet, viewMain appWithMdl ]
    }


layoutDefault : AppWithMdl -> Layout.Contents Msg
layoutDefault appWithMdl =
    { header = viewDefaultHeader appWithMdl
    , drawer = []
    , tabs = ( tabTitles, [ Color.background (Color.color Color.BlueGrey Color.S400) ] )
    , main = [ viewMain appWithMdl ]
    }


tabTitles : List (Html Msg)
tabTitles =
    Array.toList <|
        Array.map (\v -> text <| toString v) rgView


viewMain : AppWithMdl -> Html Msg
viewMain appWithMdl =
    let
        appState =
            appWithMdl.appState

        viewSelected =
            indexToView appState.viewSelected

        renderedContent =
            case viewSelected of
                Welcome ->
                    renderWelcome appWithMdl

                Dialog ->
                    App.map EditorReady (Calliope.renderDialog appState.projectActive appState.refreshEditorContent)

                Structure ->
                    Calliope.renderStructure appState.projectActive
    in
        Options.div [ css "background" "url('assets/bg.png')" ]
            [ renderedContent
            , Button.render Mdl
                [ 1 ]
                appWithMdl.mdl
                [ Button.raised
                , Button.ripple
                , Button.colored
                , Button.onClick Save
                ]
                [ text "Save" ]
            ]


renderWelcome : AppWithMdl -> Html Msg
renderWelcome appWithMdl =
    Options.div ((boxed ( 100, 20 )) ++ [ css "height" "1024px" ])
        [ grid
            []
            [ cellWelcome 2 "Create New Project" <| [ buttonNew appWithMdl ]
            , cellWelcome 5 "Recent Projects" <| renderRecentProjects appWithMdl
            , cellWelcome 5 "All Projects" <| renderAllProjects appWithMdl
            ]
        ]


buttonNew : AppWithMdl -> Html Msg
buttonNew appWithMdl =
    Button.render Mdl
        [ 2 ]
        appWithMdl.mdl
        (boxed ( 12, 0 )
            ++ [ Button.fab
               , Button.plain
               , Button.ripple
               , Button.onClick CreateNewProject
               ]
        )
        [ text "+" ]


renderRecentProjects : AppWithMdl -> List (Html Msg)
renderRecentProjects appWithMdl =
    List.map renderProjectLink appWithMdl.appState.projectsRecent


renderAllProjects : AppWithMdl -> List (Html Msg)
renderAllProjects appWithMdl =
    List.map renderProjectLink appWithMdl.appState.projectsAll


renderProjectLink : Calliope.Project -> Html Msg
renderProjectLink project =
    Options.div
        (boxed ( 0, 6 ))
        [ a [ href "", Html.onClick (OpenProject project) ] [ text project.title ] 
        , Options.div [css "float" "right"] [text project.dateCreated ]
        ] 


cellWelcome : Int -> String -> List (Html Msg) -> Cell Msg
cellWelcome gridWidth stHeader content =
    cell
        [ Grid.size All gridWidth, Elevation.e6, Color.background Color.white, css "padding" "12px" ]
        ([ Options.div
            [ Color.text <| Color.color Color.Grey Color.S700, css "margin-bottom" "12px" ]
            [ text stHeader ]
         ]
            ++ content
        )


rgView : Array View
rgView =
    Array.fromList [ Welcome, Structure, Dialog ]


viewWelcomeHeader : List (Html Msg)
viewWelcomeHeader =
    [ Layout.row
        [ css "height" "400px"
        , css "transition" "height 333ms ease-in-out 0s"
        ]
        [ Layout.title [ css "margin-right" "20px", css "color" "#000000" ] [ text "Calliope" ] ]
    ]


viewDefaultHeader : AppWithMdl -> List (Html Msg)
viewDefaultHeader appWithMdl =
    [ Layout.row [ css "transition" "height 333ms ease-in-out 0s" ]
        (if appWithMdl.appState.titleEditable then
            [ Textfield.render Mdl
                [ 0 ]
                appWithMdl.mdl
                [ Textfield.text'
                , Textfield.onInput EditTitle
                , Textfield.value appWithMdl.appState.projectActive.title
                , css "margin-right" "20px"
                ]
            , btnEditTitle "done" appWithMdl.mdl
            ]
         else
            [ Layout.title [ css "margin-right" "20px" ] [ text appWithMdl.appState.projectActive.title ]
            , btnEditTitle "mode_edit" appWithMdl.mdl
            ]
        )
    ]


btnEditTitle : String -> Material.Model -> Html Msg
btnEditTitle stIcon mdl =
    Button.render
        Mdl
        [ 0 ]
        mdl
        [ Button.icon
        , Button.ripple
        , Button.onClick ToggleEditableTitle
        ]
        [ Icon.i stIcon ]



-- STYLING

 
stylesheet : Html a
stylesheet =
    Options.stylesheet """
  .mdl-layout__header--transparent {
    background: url('assets/bg.png');
  }
"""



-- SUBS


subscriptions : AppWithMdl -> Sub Msg
subscriptions appState =
    Sub.batch
        [ getEditorContent GetEditorContent
        ]



-- ENCODE


encodeAppState : AppState -> Json.Encode.Value
encodeAppState appState =
    Json.Encode.object
        [ ( "viewSelected", Json.Encode.int appState.viewSelected )
        , ( "projectActive", Calliope.encodeProject appState.projectActive )
        , ( "projectsRecent", Json.Encode.list (List.map Calliope.encodeProject appState.projectsRecent) )
        , ( "projectsAll", Json.Encode.list (List.map Calliope.encodeProject appState.projectsAll) )
        , ( "titleEditable", Json.Encode.bool appState.titleEditable )
        , ( "refreshEditorContent", Json.Encode.bool appState.refreshEditorContent )
        ]
