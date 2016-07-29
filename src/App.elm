port module App exposing (..)

import Calliope exposing (..)
import Html exposing (..)
import Html.App as App
import Json.Encode exposing (..)
import Array exposing (..)
import Material
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options exposing (css, when)
import Material.Button as Button exposing (..)
import Debug exposing (..)


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
                wrapWithCmdNone { appWithMdl | appState = { appStateCurrent | viewSelected = index, refreshEditorContent = refreshEditorContent index } }

            ToggleEditableTitle ->
                wrapWithCmdNone { appWithMdl | appState = { appStateCurrent | titleEditable = (not appStateCurrent.titleEditable) } }

            EditTitle titleNew ->
                wrapWithCmdNone { appWithMdl | appState = { appStateCurrent | projectActive = updateProjectTitle appStateCurrent.projectActive titleNew } }

            EditorReady subMsg ->
                ( appWithMdl, configureAce "ace/theme/textmate" )

            GetEditorContent content ->
                wrapWithCmdNone { appWithMdl | appState = { appStateCurrent | projectActive = Calliope.updateProject appStateCurrent.projectActive content, refreshEditorContent = False } }

            Save ->
                ( appWithMdl
                , encodeAppState appWithMdl.appState
                    |> save
                )


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



-- VIEW


view : AppWithMdl -> Html Msg
view appWithMdl =
    Layout.render Mdl
        appWithMdl.mdl
        (layoutProperties appWithMdl.appState.viewSelected)
        (layoutContent appWithMdl)


layoutProperties : Int -> List (Layout.Property Msg)
layoutProperties viewSelected =
    [ Layout.fixedHeader
    , Layout.selectedTab viewSelected
    , Layout.onSelectTab SelectView
    ]


layoutContent : AppWithMdl -> Layout.Contents Msg
layoutContent appWithMdl =
    { header = viewHeader appWithMdl
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
        viewSelected =
            indexToView appWithMdl.appState.viewSelected

        renderedContent =
            case viewSelected of
                Welcome ->
                    renderWelcome appWithMdl

                Dialog ->
                    App.map EditorReady (Calliope.renderDialog appWithMdl.appState.projectActive appWithMdl.appState.refreshEditorContent)

                Structure ->
                    Calliope.renderStructure appWithMdl.appState.projectActive
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
    Options.div []
        [ Options.styled Html.h1
            []
            [ text "Welcome" ]
        ]


rgView : Array View
rgView =
    Array.fromList [ Welcome, Structure, Dialog ]


viewHeader : AppWithMdl -> List (Html Msg)
viewHeader appWithMdl =
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



--SUBS


subscriptions : AppWithMdl -> Sub Msg
subscriptions appState =
    Sub.batch
        [ getEditorContent GetEditorContent
        ]
