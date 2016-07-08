port module App exposing (..)

import Calliope exposing (..)
import Html exposing (..)
import Html.App as App
import Array exposing (..)
import Material
import Material.Color as Color
import Material.Textfield as Textfield
import Material.Icon as Icon
import Material.Layout as Layout
import Material.Options as Options exposing (css, when)
import Material.Button as Button exposing (..)
import Task exposing (..)


-- MODEL


type alias AppState =
    { mdl : Material.Model
    , viewSelected : Int
    , project : Project
    , titleEditable : Bool
    , refreshEditorContent : Bool
    }


type Msg
    = Mdl Material.Msg
    | SelectView Int
    | ToggleEditableTitle
    | EditTitle String
    | ConfigureAce
    | GetEditorContent String


type View
    = Welcome
    | Structure
    | Dialog



-- MAIN


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : ( AppState, Cmd Msg )
init =
    ( { mdl = Material.model
      , viewSelected = 0
      , project = Calliope.defaultProject
      , titleEditable = False
      , refreshEditorContent = False
      }
    , Layout.sub0 Mdl
    )



-- VIEW


view : AppState -> Html Msg
view appState =
    Layout.render Mdl
        appState.mdl
        (layoutProperties appState.viewSelected)
        (layoutContent appState)


layoutProperties : Int -> List (Layout.Property Msg)
layoutProperties viewSelected =
    [ Layout.fixedHeader
    , Layout.selectedTab viewSelected
    , Layout.onSelectTab SelectView
    ]


layoutContent : AppState -> Layout.Contents Msg
layoutContent appState =
    { header = viewHeader appState
    , drawer = []
    , tabs = ( tabTitles, [ Color.background (Color.color Color.BlueGrey Color.S400) ] )
    , main = [ viewMain appState ]
    }


tabTitles : List (Html Msg)
tabTitles =
    Array.toList <|
        Array.map (\v -> text <| toString v) rgView


viewMain : AppState -> Html Msg
viewMain appState =
    let
        viewSelected =
            indexToView appState.viewSelected
    in
        case viewSelected of
            Welcome ->
                renderWelcome appState

            Dialog ->
                Calliope.renderDialog appState.project appState.refreshEditorContent

            Structure ->
                Calliope.renderStructure appState.project


renderWelcome : AppState -> Html Msg
renderWelcome appState =
    Options.div []
        [ Options.styled Html.h1
            []
            [ text "Welcome" ]
        ]


rgView : Array View
rgView =
    Array.fromList [ Welcome, Structure, Dialog ]


viewHeader : AppState -> List (Html Msg)
viewHeader appState =
    [ Layout.row [ css "transition" "height 333ms ease-in-out 0s" ]
        (if appState.titleEditable then
            [ Textfield.render Mdl
                [ 0 ]
                appState.mdl
                [ Textfield.text'
                , Textfield.onInput EditTitle
                , Textfield.value appState.project.title
                , css "margin-right" "20px"
                ]
            , btnEditTitle "done" appState.mdl
            ]
         else
            [ Layout.title [ css "margin-right" "20px" ] [ text appState.project.title ]
            , btnEditTitle "mode_edit" appState.mdl
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



-- UPDATE


port configureAce : String -> Cmd m


update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg appState =
    case msg of
        SelectView i ->
            let
                ( refresh, cmd ) =
                    if ((indexToView i) == Dialog) then
                        ( True, (fx ConfigureAce) )
                        --configureAce "ace/theme/monokai"
                    else
                        ( False, Cmd.none )
            in
                ( { appState | viewSelected = i, refreshEditorContent = refresh }, cmd )

        Mdl msg ->
            Material.update Mdl msg appState

        ToggleEditableTitle ->
            wrapWithCmdNone { appState | titleEditable = (not appState.titleEditable) }

        EditTitle titleNew ->
            wrapWithCmdNone { appState | project = updateProjectTitle appState.project titleNew }

        ConfigureAce ->
            ( appState, configureAce "ace/theme/monokai" )

        GetEditorContent content ->
            wrapWithCmdNone { appState | project = Calliope.updateProject appState.project content, refreshEditorContent = False }


fx : msg -> Cmd msg
fx msg =
    Task.perform (always msg) (always msg) (Task.succeed msg)


wrapWithCmdNone : AppState -> ( AppState, Cmd Msg )
wrapWithCmdNone appState =
    ( appState, Cmd.none )


indexToView : Int -> View
indexToView i =
    Array.get i rgView |> Maybe.withDefault Welcome


updateProjectTitle : Calliope.Project -> String -> Calliope.Project
updateProjectTitle projectOld titleNew =
    { projectOld | title = titleNew }



--SUBS


port getEditorContent : (String -> msg) -> Sub msg


subscriptions : AppState -> Sub Msg
subscriptions appState =
    getEditorContent GetEditorContent
