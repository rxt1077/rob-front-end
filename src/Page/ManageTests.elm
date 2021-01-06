module Page.ManageTests exposing (..)

import Browser
import Common
    exposing
        ( bulmaButton
        , bulmaClassButton
        , bulmaClassDisabledButton
        , bulmaTextInput
        , emptyToNothing
        , handleError
        )
import Html
    exposing
        ( Html
        , a
        , button
        , div
        , footer
        , h1
        , header
        , input
        , label
        , p
        , section
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode



-- MODEL


type alias Test =
    { id : Int
    , name : String
    , description : String
    , memsnapshot : Maybe String
    , testsImage : Maybe String
    , baseImage : String
    , command : String
    , commandTimeout : Int
    , prompt : String
    , jobs : List Int
    }


type alias ModalData =
    { test : Test
    , title : String
    , onClick : Test -> Msg
    , buttonText : String
    , timeString : String
    }


type alias Model =
    { tests : List Test
    , modalData : Maybe ModalData
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { tests = []
      , modalData = Nothing
      , error = Nothing
      }
    , getTests GotTests
    )



-- UPDATE


type Msg
    = GotTests (Result Http.Error (List Test))
    | EditTestButton Test
    | DeleteTestButton Test
    | AddTestButton
    | CancelEditButton
    | TextInput Test
    | TimeoutInput String
    | CreateTest Test
    | ReloadTests (Result Http.Error ())
    | UpdateTest Test


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTests result ->
            case result of
                Ok tests ->
                    ( { model | tests = tests, error = Nothing }, Cmd.none )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        EditTestButton test ->
            ( { model
                | modalData =
                    Just
                        { test = test
                        , title = "Editing test " ++ String.fromInt test.id
                        , onClick = UpdateTest
                        , buttonText = "Update"
                        , timeString = String.fromInt test.commandTimeout
                        }
              }
            , Cmd.none
            )

        DeleteTestButton test ->
            ( model, deleteTest test.id )

        AddTestButton ->
            let
                emptyTest =
                    { id = 0
                    , name = ""
                    , description = ""
                    , memsnapshot = Just "images/memsnapshot.gz"
                    , testsImage = Just "tests/tests.iso"
                    , baseImage = "images/debian-10.qcow2"
                    , command = ""
                    , commandTimeout = 300
                    , prompt = "root@rob-run:~#"
                    , jobs = []
                    }

                modalData =
                    Just
                        { test = emptyTest
                        , title = "Create a new test"
                        , onClick = CreateTest
                        , buttonText = "Save"
                        , timeString = "300"
                        }
            in
            ( { model | modalData = modalData }, Cmd.none )

        CancelEditButton ->
            ( { model | modalData = Nothing }, Cmd.none )

        TextInput test ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    ( { model | modalData = Just { modalData | test = test } }, Cmd.none )

        TimeoutInput timeString ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    let
                        test =
                            modalData.test
                    in
                    case timeString of
                        "" ->
                            ( { model | modalData = Just { modalData | timeString = "", test = { test | commandTimeout = 0 } } }, Cmd.none )

                        _ ->
                            case String.toInt timeString of
                                Nothing ->
                                    ( model, Cmd.none )

                                Just timeout ->
                                    ( { model | modalData = Just { modalData | timeString = timeString, test = { test | commandTimeout = timeout } } }, Cmd.none )

        CreateTest test ->
            ( { model | modalData = Nothing }, createTest test )

        ReloadTests result ->
            case result of
                Ok _ ->
                    ( model, getTests GotTests )

                Err error ->
                    ( model, Cmd.none )

        UpdateTest test ->
            ( { model | modalData = Nothing }, updateTest test )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- provides HTML for a test edit modal


editModal : ModalData -> Html Msg
editModal data =
    let
        test =
            data.test
    in
    div [ class "modal", class "is-active" ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ text data.title ]
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "columns" ]
                    [ div [ class "column", class "is-one-third" ]
                        [ bulmaTextInput "Name"
                            (\n -> TextInput { test | name = n })
                            test.name
                        ]
                    , div [ class "column" ]
                        [ bulmaTextInput "Description"
                            (\n -> TextInput { test | description = n })
                            test.description
                        ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ bulmaTextInput "Base Image"
                            (\n -> TextInput { test | baseImage = n })
                            test.baseImage
                        , bulmaTextInput "Prompt"
                            (\n -> TextInput { test | prompt = n })
                            test.prompt
                        ]
                    , div [ class "column" ]
                        [ bulmaTextInput "Tests ISO"
                            (\n -> TextInput { test | testsImage = emptyToNothing n })
                            (Maybe.withDefault "" test.testsImage)
                        , bulmaTextInput "Command"
                            (\n -> TextInput { test | command = n })
                            test.command
                        ]
                    , div [ class "column" ]
                        [ bulmaTextInput "Memory Snapshot"
                            (\n -> TextInput { test | memsnapshot = emptyToNothing n })
                            (Maybe.withDefault "" test.memsnapshot)
                        , bulmaTextInput "Timeout (seconds)" TimeoutInput data.timeString
                        ]
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ div [ class "field", class "is-grouped" ]
                    [ bulmaClassButton data.buttonText (data.onClick test) "is-success"
                    , bulmaButton "Cancel" CancelEditButton
                    ]
                ]
            ]
        ]



-- provides HTML for the Edit and Delete buttons at the end of a row


rowControls : Test -> Html Msg
rowControls test =
    div [ class "field", class "is-grouped" ]
        [ bulmaClassButton "Edit" (EditTestButton test) "is-link"
        , case test.jobs of
            [] ->
                bulmaClassButton "Delete" (DeleteTestButton test) "is-danger"

            _ ->
                bulmaClassDisabledButton "Delete" "is-danger"
        ]



-- provides HTML for a single row of the table


rowItem : Test -> Html Msg
rowItem test =
    tr []
        [ td [] [ text (String.fromInt test.id) ]
        , td [] [ text test.name ]
        , td [] [ text test.description ]
        , td [] [ text test.command ]
        , td [] [ rowControls test ]
        ]



-- provides HTML for the main table


testTable : List Test -> Html Msg
testTable tests =
    table [ class "table", class "is-hoverable", class "is-fullwidth" ]
        [ thead []
            [ tr []
                [ th [] [ text "ID" ]
                , th [] [ text "Name" ]
                , th [] [ text "Description" ]
                , th [] [ text "Command" ]
                , th []
                    [ div [] [ text "Actions" ]
                    , div [ class "is-size-7" ] [ text "in-use tests can't be deleted" ]
                    ]
                ]
            ]
        , tbody [] (List.map rowItem tests)
        ]



-- provides HTML for an add test button


addTestButton : Html Msg
addTestButton =
    div [ class "field", class "is-grouped" ]
        [ bulmaClassButton "Add test" AddTestButton "is-primary" ]



-- provides HTML for the main view


view : Model -> Html Msg
view model =
    div []
        [ case model.modalData of
            Nothing ->
                div [] []

            Just modalData ->
                editModal modalData
        , case model.error of
            Nothing ->
                div [] []

            Just error ->
                div [ class "notification", class "is-warning" ] [ text error ]
        , h1 [ class "title" ] [ text "Manage Tests" ]
        , testTable model.tests
        , addTestButton
        ]



-- HTTP


getTests : (Result Http.Error (List Test) -> msg) -> Cmd msg
getTests event =
    Http.get
        { url = "/test"
        , expect = Http.expectJson event testsDecoder
        }


testsDecoder : Decoder (List Test)
testsDecoder =
    list
        (Decode.succeed Test
            |> required "id" int
            |> required "name" string
            |> required "description" string
            |> required "memsnapshot" (nullable string)
            |> required "tests_image" (nullable string)
            |> required "base_image" string
            |> required "command" string
            |> required "command_timeout" int
            |> required "prompt" string
            |> required "jobs" (list int)
        )


createTest : Test -> Cmd Msg
createTest test =
    Http.post
        { url = "/test"
        , body = Http.jsonBody (testEncoder test)
        , expect = Http.expectWhatever ReloadTests
        }


nullableString : Maybe String -> Encode.Value
nullableString maybeStr =
    case maybeStr of
        Just str ->
            Encode.string str

        Nothing ->
            Encode.null


testEncoder : Test -> Encode.Value
testEncoder test =
    Encode.object
        [ ( "id", Encode.int test.id )
        , ( "name", Encode.string test.name )
        , ( "description", Encode.string test.description )
        , ( "memsnapshot", nullableString test.memsnapshot )
        , ( "tests_image", nullableString test.testsImage )
        , ( "base_image", Encode.string test.baseImage )
        , ( "command", Encode.string test.command )
        , ( "command_timeout", Encode.int test.commandTimeout )
        , ( "prompt", Encode.string test.prompt )
        , ( "jobs", Encode.list Encode.int test.jobs )
        ]


deleteTest : Int -> Cmd Msg
deleteTest id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/test/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever ReloadTests
        , timeout = Nothing
        , tracker = Nothing
        }


updateTest : Test -> Cmd Msg
updateTest test =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/test/" ++ String.fromInt test.id
        , body = Http.jsonBody (testEncoder test)
        , expect = Http.expectWhatever ReloadTests
        , timeout = Nothing
        , tracker = Nothing
        }
