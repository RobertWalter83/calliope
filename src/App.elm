port module App exposing (..)

import Calliope exposing (..)
import Task exposing (..)
import Time exposing (..)
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
    = Mdl (Material.Msg Msg)
    | SelectView Int
    | ToggleEditableTitle 
    | EditTitle String
    | GetEditorContent String
    | EditorReady Calliope.Msg
    | Save
    | CreateNewProject
    | OpenProject Project
    | CreateNewProjectNow Time 
    | NoOp



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
                    wrapWithMdl addDefaultProject

                Maybe.Just appState ->
                    wrapWithMdl { appState | refreshEditorContent = True }
    in
        ( appStateInit, Layout.sub0 Mdl )


wrapWithMdl : AppState -> AppWithMdl
wrapWithMdl appState =
    { mdl = Material.model
    , appState = appState
    } 

addDefaultProject : AppState
addDefaultProject =
    addNewProject Calliope.defaultProject


addNewProject : Project -> AppState
addNewProject project =
    { viewSelected = 0
    , projectActive = project
    , projectsAll = []
    , projectsRecent = []
    , titleEditable = False
    , refreshEditorContent = False
    }

    
cmdTimeNow : Cmd Msg
cmdTimeNow = Task.perform (\_ -> NoOp) CreateNewProjectNow Time.now



-- UPDATE


update : Msg -> AppWithMdl -> ( AppWithMdl, Cmd Msg )
update msg appWithMdl =
    let
        appStateCurrent =
            appWithMdl.appState
    in
        case msg of
            Mdl msg ->
                Material.update msg appWithMdl

            SelectView index ->
                selectView appStateCurrent index |> appMdlWithCmdNone appWithMdl  

            ToggleEditableTitle ->
                toggleEditableTitle appStateCurrent |> appMdlWithCmdNone appWithMdl

            EditTitle titleNew ->
                editTitle appStateCurrent titleNew |> appMdlWithCmdNone appWithMdl

            EditorReady subMsg ->
                ( appWithMdl, configureAce "ace/theme/textmate" )

            GetEditorContent content ->
                setEditorContent appStateCurrent content |> appMdlWithCmdNone appWithMdl

            Save ->
                ( appWithMdl
                , encodeAppState appWithMdl.appState
                    |> save
                )

            CreateNewProjectNow timeNow -> 
                createNewProject appStateCurrent timeNow |> appMdlWithCmdNone appWithMdl

            CreateNewProject ->
                ( appWithMdl, cmdTimeNow )
                
            OpenProject project ->
                openProject appStateCurrent project |> appMdlWithCmdNone appWithMdl

            NoOp -> 
                appMdlWithCmdNone appWithMdl appWithMdl.appState


selectView : AppState -> Int -> AppState
selectView appStateCurrent index =
    { appStateCurrent | viewSelected = index, refreshEditorContent = refreshEditorContent index }


toggleEditableTitle : AppState -> AppState
toggleEditableTitle appStateCurrent =
    { appStateCurrent | titleEditable = (not appStateCurrent.titleEditable) }


editTitle : AppState -> String -> AppState
editTitle appStateCurrent titleNew =
    let
        newProject = updateProjectTitle appStateCurrent.projectActive titleNew

        projectsRecent = newProject ::
            (Debug.log "projectsRecent" filter appStateCurrent.projectsRecent (Debug.log "projectActive" appStateCurrent.projectActive))

        projectsAll = newProject ::
            filter appStateCurrent.projectsAll appStateCurrent.projectActive 
    in

    { appStateCurrent | projectActive = newProject, projectsRecent = projectsRecent, projectsAll = projectsAll }


filter : List a -> a -> List a
filter list item =
    List.filter (\i -> i /= item) list

setEditorContent : AppState -> String -> AppState
setEditorContent appStateCurrent content =
    { appStateCurrent | projectActive = Calliope.updateProject appStateCurrent.projectActive content, refreshEditorContent = False }


createNewProject : AppState -> Time -> AppState
createNewProject appStateCurrent timeNow =
    let
        newProject = Calliope.createProject timeNow

        projectsRecent =
            newProject :: appStateCurrent.projectsRecent

        projectsAll =
            newProject :: appStateCurrent.projectsAll 

        appStateNew = addNewProject newProject 
    in
        { appStateNew | projectsRecent = projectsRecent, projectsAll = projectsAll }


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
            filter projectsCurrent projectNew
    in
        projectNew :: filtered


refreshEditorContent : Int -> Bool
refreshEditorContent index =
    if ((indexToView index) == Dialog) then
        True
    else
        False


appMdlWithCmdNone : AppWithMdl -> AppState -> ( AppWithMdl, Cmd Msg )
appMdlWithCmdNone appWithMdl appStateNew =
    ( { appWithMdl | appState = appStateNew }, Cmd.none )


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
            , cellWelcome 5 "Recent Projects" <| renderProjectList appWithMdl.appState.projectsRecent
            , cellWelcome 5 "All Projects" <| renderProjectList appWithMdl.appState.projectsAll
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


renderProjectList : List Project -> List (Html Msg)
renderProjectList projects =
    List.map renderProjectLink projects


renderProjectLink : Calliope.Project -> Html Msg
renderProjectLink project =
    Options.div
        (boxed ( 0, 6 ))
        [ a [ href "", Html.onClick (OpenProject project) ] [ text project.title ] 
        , Options.div [css "float" "right", css "margin-left" "4px"] [text project.timeCreated ]
        , Options.div [css "float" "right", css "margin-left" "4px"] [text project.dateCreated ]
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
        [ css "height" "320px"
        , css "min-height" "320px"
        , css "max-height" "320px"
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
    background: url('assets/march.jpg') 0 45% no-repeat;
    background-size: 100% auto
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
