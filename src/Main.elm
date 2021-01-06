module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Common exposing (handleError)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, bool, field, map2, string)
import Page.About as About
import Page.Jobs as Jobs
import Page.ManageGroups as ManageGroups
import Page.ManageTests as ManageTests
import Url



-- TODO: Make the URL look like /rob instead of /rob/index.html
-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Session =
    { ucid : String
    , instructor : Bool
    }


type Page
    = NotFound
    | About About.Model
    | Jobs Jobs.Model
    | ManageGroups ManageGroups.Model
    | ManageTests ManageTests.Model


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , page : Page
    , session : Session
    , error : Maybe String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( model, cmd ) =
            runPage
                { key = key
                , url = url
                , page = NotFound
                , session = { ucid = "None", instructor = False }
                , error = Nothing
                }
                (About.init ())
                -- we start on the About page
                About
                AboutMsg
    in
    ( model, Cmd.batch [ cmd, getSession ] )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotSession (Result Http.Error Session)
    | AboutMsg About.Msg
    | JobsMsg Jobs.Msg
    | ManageGroupsMsg ManageGroups.Msg
    | ManageTestsMsg ManageTests.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "Msg" msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                updatedModel =
                    { model | url = url }
            in
            case url.path of
                -- very simple for now, no URL parser
                "/rob/about" ->
                    runPage updatedModel (About.init ()) About AboutMsg

                "/rob/jobs" ->
                    runPage updatedModel (Jobs.init ()) Jobs JobsMsg

                "/rob/groups" ->
                    runPage updatedModel (ManageGroups.init ()) ManageGroups ManageGroupsMsg

                "/rob/tests" ->
                    runPage updatedModel (ManageTests.init ()) ManageTests ManageTestsMsg

                _ ->
                    ( { updatedModel | page = NotFound }, Cmd.none )

        GotSession result ->
            case result of
                Ok session ->
                    ( { model | session = session, error = Nothing }, Cmd.none )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        AboutMsg aboutMsg ->
            case model.page of
                About aboutModel ->
                    runPage model (About.update aboutMsg aboutModel) About AboutMsg

                _ ->
                    ( model, Cmd.none )

        JobsMsg jobsMsg ->
            case model.page of
                Jobs jobsModel ->
                    runPage model (Jobs.update jobsMsg jobsModel) Jobs JobsMsg

                _ ->
                    ( model, Cmd.none )

        ManageGroupsMsg manageGroupsMsg ->
            case model.page of
                ManageGroups manageGroupsModel ->
                    runPage
                        model
                        (ManageGroups.update manageGroupsMsg manageGroupsModel)
                        ManageGroups
                        ManageGroupsMsg

                _ ->
                    ( model, Cmd.none )

        ManageTestsMsg manageTestsMsg ->
            case model.page of
                ManageTests manageTestsModel ->
                    runPage
                        model
                        (ManageTests.update manageTestsMsg manageTestsModel)
                        ManageTests
                        ManageTestsMsg

                _ ->
                    ( model, Cmd.none )


{-| Run a function for a particular Page converting the response back to our
( Model, Cmd Msg ). Typically used with the update and init functions.

runPage model (About.init ()) About AboutMsg

-}
runPage : Model -> ( model, Cmd msg ) -> (model -> Page) -> (msg -> Msg) -> ( Model, Cmd Msg )
runPage model ( pageModel, pageMsg ) pageType msgType =
    ( { model | page = pageType pageModel }
    , Cmd.map msgType pageMsg
    )



-- SUBSCRIPTIONS
-- currently only the Jobs page uses subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Jobs subModel ->
            Sub.map JobsMsg (Jobs.subscriptions subModel)

        _ ->
            Sub.none



-- VIEW


navbarItem : String -> String -> Html Msg
navbarItem name link =
    a [ class "navbar-item", href link ] [ text name ]


instructorMenu : String -> Html Msg
instructorMenu ucid =
    div [ class "navbar-menu" ]
        [ p [ class "navbar-start" ]
            [ navbarItem "About" "about"
            , navbarItem "Jobs" "jobs"
            , navbarItem "Tests" "tests"
            , navbarItem "Groups" "groups"
            ]
        , p [ class "navbar-end" ]
            [ p [ class "navbar-item" ] [ text "Instructor" ]
            , p [ class "navbar-item" ] [ text ucid ]
            ]
        ]


studentMenu : String -> Html Msg
studentMenu ucid =
    div [ class "navbar-menu" ]
        [ p [ class "navbar-start" ]
            [ navbarItem "About" "about"
            , navbarItem "Student View" "student"
            ]
        , p [ class "navbar-end" ] [ p [ class "navbar-item" ] [ text ucid ] ]
        ]


navbar : Session -> Html Msg
navbar session =
    nav [ class "navbar" ]
        [ div [ class "navbar-brand" ]
            [ div [ class "navbar-item", class "has-text-weight-bold" ]
                [ text "Rob the Builder" ]
            ]
        , if session.instructor then
            instructorMenu session.ucid

          else
            studentMenu session.ucid
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Rob the Builder"
    , body =
        [ navbar model.session
        , section [ class "section" ]
            [ case model.error of
                Nothing ->
                    div [] []

                Just error ->
                    div [ class "notification", class "is-warning" ] [ text error ]
            , div [ class "container" ]
                [ case model.page of
                    NotFound ->
                        div [] [ text "Page not found" ]

                    About subModel ->
                        Html.map AboutMsg (About.view subModel)

                    Jobs subModel ->
                        Html.map JobsMsg (Jobs.view subModel)

                    ManageGroups subModel ->
                        Html.map ManageGroupsMsg (ManageGroups.view subModel)

                    ManageTests subModel ->
                        Html.map ManageTestsMsg (ManageTests.view subModel)
                ]
            ]
        ]
    }



-- HTTP


getSession : Cmd Msg
getSession =
    Http.get
        { url = "/session"
        , expect = Http.expectJson GotSession sessionDecoder
        }


sessionDecoder : Decoder Session
sessionDecoder =
    map2 Session
        (field "ucid" string)
        (field "instructor" bool)
