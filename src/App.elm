module App exposing (..)


import Ports exposing (..)
import Project exposing (..)
import Task exposing (..)
import Time exposing (..)
import Html exposing (..)
import Html.Events as Html exposing (..)
import Html.App as App 
import Json.Encode exposing (..)
import Array exposing (..)
import Material
import Material.Helpers exposing(..)
import Material.Card as Card
import Material.Color as Color
import Material.Layout as Layout
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
    , refreshEditorContent : Bool
    , raisedCard : Int
    }


type alias AppWithMdl =
    { mdl : Material.Model
    , appState : AppState
    }


type alias PolaroidParams =
    { index : Int, appWithMdl : AppWithMdl, onClick : Msg, pathBackground : String, messageTuple : ( String, String ) }

type alias Mdl = 
  Material.Model 

type Msg
    = Mdl (Material.Msg Msg)
    | UpdateContent Project.Msg
    | SelectView Int
    | TitleEditable Project.Msg
    | EditorReady Project.Msg
    | Save
    | CreateNewProject
    | OpenProject Project
    | CreateNewProjectNow Time
    | Raise Int
    | NoOp


type View
    = Overview
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


init : ( AppWithMdl, Cmd Msg )
init =
   ( wrapWithMdl addDefaultProject, Layout.sub0 Mdl )


wrapWithMdl : AppState -> AppWithMdl
wrapWithMdl appState =
    { mdl = Material.model  
    , appState = appState
    }


addDefaultProject : AppState
addDefaultProject =
    addNewProject Project.defaultProject


addNewProject : Project -> AppState
addNewProject project =
    { viewSelected = 0
    , projectActive = project
    , projectsAll = []
    , projectsRecent = []
    , refreshEditorContent = False
    , raisedCard = -1
    }


cmdTimeNow : Cmd Msg
cmdTimeNow =
    Task.perform (\_ -> NoOp) CreateNewProjectNow Time.now



-- UPDATE


update : Msg -> AppWithMdl -> ( AppWithMdl, Cmd Msg )
update msg appWithMdl =
    let
        appStateCurrent =
            appWithMdl.appState
    in
        case msg of
            Mdl msg' ->
                Material.update msg' appWithMdl

            UpdateContent subMsg ->
                ( appWithMdl, Cmd.none )

            Raise cardIndex ->
                raiseCard appStateCurrent cardIndex |> appMdlWithCmdNone appWithMdl

            SelectView index ->
                selectView appStateCurrent index |> appMdlWithCmdNone appWithMdl

            EditorReady projectMsg ->
                ( appWithMdl, configureAce "ace/theme/textmate" )
 
            TitleEditable projectMsg -> 
                let (appStateNew, cmd) = lift .projectActive (\a p->{a|projectActive   =p}) TitleEditable Project.update projectMsg appStateCurrent
                in appMdlWithCmdNone appWithMdl appStateNew

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


raiseCard : AppState -> Int -> AppState
raiseCard appStateCurrent cardIndex =
    { appStateCurrent | raisedCard = cardIndex }


selectView : AppState -> Int -> AppState
selectView appStateCurrent index =
    { appStateCurrent | viewSelected = index, refreshEditorContent = refreshEditorContent index }

{-
editTitle : AppState -> String -> AppState
editTitle appStateCurrent titleNew =
    let
        newProject =
            updateProjectTitle appStateCurrent.projectActive titleNew

        projectsRecent =
            newProject
                :: (Debug.log "projectsRecent" filter appStateCurrent.projectsRecent (Debug.log "projectActive" appStateCurrent.projectActive))

        projectsAll =
            newProject
                :: filter appStateCurrent.projectsAll appStateCurrent.projectActive
    in
        { appStateCurrent | projectActive = newProject, projectsRecent = projectsRecent, projectsAll = projectsAll }
-}

filter : List a -> a -> List a
filter list item =
    List.filter (\i -> i /= item) list


createNewProject : AppState -> Time -> AppState
createNewProject appStateCurrent timeNow =
    let
        newProject =
            Project.createProject timeNow

        projectsRecent =
            newProject :: appStateCurrent.projectsRecent
 
        projectsAll = 
            sortByTitle <| newProject :: appStateCurrent.projectsAll

        appStateNew =
            addNewProject newProject
    in
        { appStateNew | projectsRecent = projectsRecent, projectsAll = projectsAll }


openProject : AppState -> Project -> AppState
openProject appStateCurrent projectToOpen =
    let
        projectsRecent = 
            updateProjectList appStateCurrent.projectsRecent projectToOpen

        projectsAll = 
            sortByTitle <| updateProjectList appStateCurrent.projectsAll projectToOpen
    in
        { appStateCurrent | projectActive = projectToOpen, projectsRecent = projectsRecent, projectsAll = projectsAll, viewSelected = 1 }


sortByTitle : List Project -> List Project
sortByTitle input =
  List.sortBy (\p -> p.projectData.title) input


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
    Array.get i rgView |> Maybe.withDefault Overview




-- VIEW


view : AppWithMdl -> Html Msg
view appWithMdl =
    let
        layoutContent =
            if ((indexToView appWithMdl.appState.viewSelected) == Overview) then
                layoutOverview
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
    , if ((indexToView viewSelected) == Overview) then
        Layout.transparentHeader
      else
        Options.nop
    ]


layoutOverview : AppWithMdl -> Layout.Contents Msg
layoutOverview appWithMdl =
    { header = viewOverviewHeader
    , drawer = []
    , tabs = ( [], [] )
    , main = [ stylesheet, viewMain appWithMdl ]
    }


layoutDefault : AppWithMdl -> Layout.Contents Msg
layoutDefault appWithMdl =
    { header = viewDefaultHeader appWithMdl
    , drawer = []
    , tabs = ( tabTitles, [ Color.background (Color.color Color.Teal Color.S600) ] )
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
                Overview -> 
                    renderOverview appWithMdl

                Dialog -> 
                    App.map EditorReady (Project.renderDialog appState.projectActive appState.refreshEditorContent)

                Structure ->
                    Project.renderStructure appState.projectActive
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


renderOverview : AppWithMdl -> Html Msg
renderOverview appWithMdl =
    let
        lengthRecentProjects =
            List.length appWithMdl.appState.projectsRecent
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
                    [ renderPolaroid appWithMdl.appState.projectActive <|
                        createParams 0 appWithMdl CreateNewProject "assets/new.jpg" ( "New Project", "Create a brand new project" )
                    ]
                 ]
                    ++ (List.map2 renderProjectLink
                            appWithMdl.appState.projectsRecent
                        <|
                            List.map5 createParams
                                [1..lengthRecentProjects]
                                (List.repeat lengthRecentProjects appWithMdl)
                                (List.map (\p -> OpenProject p) appWithMdl.appState.projectsRecent)
                                (List.repeat lengthRecentProjects "assets/existing.jpg")
                                (List.map (\p -> ( p.projectData.title, "Click here to open." )) appWithMdl.appState.projectsRecent)
                       )
                )
            ]


createParams : Int -> AppWithMdl -> Msg -> String -> ( String, String ) -> PolaroidParams
createParams index appWithMdl msg pathBackground messageTuple =
    { index = index
    , appWithMdl = appWithMdl
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
            [ if params.appWithMdl.appState.raisedCard == params.index then
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
                    [  ]
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


rgView : Array View
rgView =
    Array.fromList [ Overview, Structure, Dialog ]


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


viewDefaultHeader : AppWithMdl -> List (Html Msg)
viewDefaultHeader appWithMdl =  
    [ Layout.row [ css "transition" "height 333ms ease-in-out 0s" ]
        [ App.map TitleEditable (Project.renderProjectTitle appWithMdl.appState.projectActive ) ]
    ]
 


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
subscriptions appWithMdl =
    Sub.batch 
      [ Sub.map UpdateContent <| Project.subscriptions appWithMdl.appState.projectActive ]

 

-- ENCODE


encodeAppState : AppState -> Json.Encode.Value
encodeAppState appState =
    Json.Encode.object
        [ ( "viewSelected", Json.Encode.int appState.viewSelected )
        , ( "projectActive", Project.encodeProject appState.projectActive )
        , ( "projectsRecent", Json.Encode.list (List.map Project.encodeProject appState.projectsRecent) )
        , ( "projectsAll", Json.Encode.list (List.map Project.encodeProject appState.projectsAll) )
        , ( "refreshEditorContent", Json.Encode.bool appState.refreshEditorContent )
        , ( "raisedCard", Json.Encode.int appState.raisedCard )
        ]
