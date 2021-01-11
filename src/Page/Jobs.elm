module Page.Jobs exposing (..)

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
        , i
        , input
        , label
        , option
        , p
        , pre
        , section
        , select
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class, classList, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder, int, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Page.ManageGroups exposing (Group, getGroups)
import Page.ManageTests exposing (Test, getTests)
import Task
import Time exposing (Posix)



-- MODEL


type alias DropDown =
    { label : String
    , event : String -> Msg
    , choices : List ( String, String )
    }


setChoices : List ( String, String ) -> DropDown -> DropDown
setChoices choices dropdown =
    { dropdown | choices = choices }


toChoice : String -> Int -> ( String, String )
toChoice name id =
    let
        idStr =
            String.fromInt id

        friendly =
            idStr ++ ": " ++ name
    in
    ( friendly, idStr )


testsToChoices : List Test -> List ( String, String )
testsToChoices tests =
    List.map (\t -> toChoice t.name t.id) tests


groupsToChoices : List Group -> List ( String, String )
groupsToChoices groups =
    List.map (\g -> toChoice g.name g.id) groups


type alias Job =
    { id : Int
    , created : Posix
    , started : Maybe Posix
    , completed : Maybe Posix
    , status : String
    , groupID : Int
    , testID : Int
    , output : String
    }


type alias Model =
    { jobs : List Job
    , groups : List Group
    , tests : List Test
    , error : Maybe String
    , groupDropDown : DropDown
    , testDropDown : DropDown
    , groupID : Maybe Int
    , testID : Maybe Int
    , details : Maybe Job
    , timezone : Time.Zone
    , lastUpdated : Posix
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { jobs = []
      , groups = []
      , tests = []
      , error = Nothing
      , groupDropDown =
            { label = "Group"
            , choices = []
            , event = GroupInput
            }
      , testDropDown =
            { label = "Test"
            , choices = []
            , event = TestInput
            }
      , groupID = Nothing
      , testID = Nothing
      , details = Nothing
      , timezone = Time.utc
      , lastUpdated = Time.millisToPosix 0
      }
    , Cmd.batch
        [ getJobs
        , getTests GotTests
        , getGroups GotGroups
        , Task.perform GotTimezone Time.here
        ]
    )



-- UPDATE


type Msg
    = GotTime Posix
    | GotTimezone Time.Zone
    | GotJobs (Result Http.Error (List Job))
    | GotTests (Result Http.Error (List Test))
    | GotGroups (Result Http.Error (List Group))
    | AddJobButton
    | DetailsToggle Job
    | DeleteJobButton Job
    | GroupInput String
    | TestInput String
    | ReloadJobs (Result Http.Error ())
    | Refresh Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "MSG" msg of
        GotTime time ->
            ( { model | lastUpdated = time }, Cmd.none )

        GotTimezone timezone ->
            ( { model | timezone = timezone }, Cmd.none )

        GotJobs result ->
            case result of
                Ok jobs ->
                    ( { model
                        | jobs = jobs
                        , error = Nothing
                      }
                    , Task.perform GotTime Time.now
                    )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        GotTests result ->
            case result of
                Ok tests ->
                    ( { model
                        | testDropDown =
                            setChoices (testsToChoices tests) model.testDropDown
                        , tests = tests
                      }
                    , Cmd.none
                    )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        GotGroups result ->
            case result of
                Ok groups ->
                    ( { model
                        | groupDropDown =
                            setChoices (groupsToChoices groups) model.groupDropDown
                        , groups = groups
                      }
                    , Cmd.none
                    )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        DetailsToggle job ->
            case model.details of
                Nothing ->
                    ( { model | details = Just job }, Cmd.none )

                Just _ ->
                    ( { model | details = Nothing }, Cmd.none )

        DeleteJobButton job ->
            ( model, deleteJob job.id )

        GroupInput groupID ->
            ( { model | groupID = String.toInt groupID }, Cmd.none )

        TestInput testID ->
            ( { model | testID = String.toInt testID }, Cmd.none )

        AddJobButton ->
            case ( model.groupID, model.testID ) of
                ( Just groupID, Just testID ) ->
                    ( model
                    , createJob
                        { id = 0
                        , created = Time.millisToPosix 0
                        , started = Nothing
                        , completed = Nothing
                        , status = "QUEUED"
                        , groupID = groupID
                        , testID = testID
                        , output = ""
                        }
                    )

                _ ->
                    ( { model | error = Just "Please select a group and test" }
                    , Cmd.none
                    )

        ReloadJobs result ->
            case result of
                Ok _ ->
                    ( model, getJobs )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        Refresh _ ->
            ( model, getJobs )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every (10 * 1000) Refresh ]



-- VIEW
-- provides HTML for the Edit and Delete buttons at the end of a row


rowControls : Job -> Html Msg
rowControls job =
    div [ class "field", class "is-grouped" ]
        [ bulmaClassButton "Details" (DetailsToggle job) "is-link"
        , case job.status of
            "RUNNING" ->
                bulmaClassDisabledButton "Delete" "is-danger"

            _ ->
                bulmaClassButton "Delete" (DeleteJobButton job) "is-danger"
        ]


datetimeOrNone : Maybe Posix -> String
datetimeOrNone maybePosix =
    case maybePosix of
        Nothing ->
            "None"

        Just posix ->
            Iso8601.fromTime posix



-- provides HTML for a single row of the table


rowItem : Job -> Model -> Html Msg
rowItem job model =
    tr []
        [ td [] [ text (String.fromInt job.id) ]
        , td [] [ text (toDateTime model.timezone job.created) ]
        , td [] [ text (groupNameFromID job.groupID model.groups) ]
        , td [] [ text (testNameFromID job.testID model.tests) ]
        , td [] [ text job.status ]
        , td [] [ rowControls job ]
        ]



-- provides HTML for the main table


jobsTable : Model -> Html Msg
jobsTable model =
    table [ class "table", class "is-hoverable", class "is-fullwidth" ]
        [ thead []
            [ tr []
                [ th [] [ text "ID" ]
                , th [] [ text "Created" ]
                , th [] [ text "Group" ]
                , th [] [ text "Test" ]
                , th [] [ text "Status" ]
                , th []
                    [ div [] [ text "Actions" ]
                    , div [ class "is-size-7" ] [ text "RUNNING jobs cannot be deleted" ]
                    ]
                ]
            ]
        , tbody [] (List.map (\j -> rowItem j model) model.jobs)
        ]



-- provides HTML for a select option


selectOption : ( String, String ) -> Html Msg
selectOption ( name, val ) =
    option [ value val ] [ text name ]



-- provides HTML for a dropdown
-- I realize the inline styles are ugly, but it's the only way I could get a
-- horizontal form with multiple labels to work


dropDown : DropDown -> List (Html Msg)
dropDown dropdown =
    [ div [ class "field-label is-normal", style "flex-grow" "0" ]
        [ label [ class "label" ] [ text dropdown.label ] ]
    , div
        [ class "field-body"
        , style "flex-grow" "0"
        , style "margin-right" "1.5rem"
        ]
        [ div [ class "field" ]
            [ p [ class "control" ]
                [ div [ class "select" ]
                    [ select [ onInput dropdown.event ]
                        (option [ value "" ] [ text "Please select..." ]
                            :: List.map selectOption dropdown.choices
                        )
                    ]
                ]
            ]
        ]
    ]



-- provides HTML for the add jobs form


addJob : Model -> Html Msg
addJob model =
    div [ class "field is-horizontal" ]
        (dropDown model.groupDropDown
            ++ dropDown model.testDropDown
            ++ [ bulmaClassButton "Add job" AddJobButton "is-primary" ]
        )


detailsItem : String -> String -> Html Msg
detailsItem label value =
    div []
        [ span [ class "has-text-weight-bold" ] [ text (label ++ " ") ]
        , text value
        ]


groupNameFromID : Int -> List Group -> String
groupNameFromID groupID groups =
    case List.filter (\g -> g.id == groupID) groups of
        [] ->
            "Group not found!"

        [ group ] ->
            String.fromInt group.id ++ ": " ++ group.name

        _ ->
            "Multiple groups found!"


testNameFromID : Int -> List Test -> String
testNameFromID testID tests =
    case List.filter (\t -> t.id == testID) tests of
        [] ->
            "Test not found!"

        [ test ] ->
            String.fromInt test.id ++ ": " ++ test.name

        _ ->
            "Multiple tests found!"



-- provides HTML for a modal showing the details of a job
-- TODO: Provide group and test name as well as ID


detailsModal : Job -> List Group -> List Test -> Html Msg
detailsModal job groups tests =
    div [ class "modal is-active" ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ] [ text "Job Details" ]
                , button [ class "delete", onClick (DetailsToggle job) ] []
                ]
            , section [ class "modal-card-body" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ detailsItem "ID" (String.fromInt job.id)
                        , detailsItem
                            "Group"
                            (groupNameFromID job.groupID groups)
                        ]
                    , div [ class "column" ]
                        [ detailsItem "Status" job.status
                        , detailsItem
                            "Test"
                            (testNameFromID job.testID tests)
                        ]
                    ]
                , div [ class "columns" ]
                    [ div [ class "column" ]
                        [ detailsItem "Created" (Iso8601.fromTime job.created) ]
                    , div [ class "column" ]
                        [ detailsItem "Started" (datetimeOrNone job.started) ]
                    , div [ class "column" ]
                        [ detailsItem "Completed" (datetimeOrNone job.completed) ]
                    ]
                , div [ class "has-text-weight-bold" ] [ text "Output" ]
                , pre [] [ text job.output ]
                ]
            , footer [ class "modal-card-foot" ] []
            ]
        ]


padTime : Int -> String
padTime time =
    String.padLeft 2 '0' (String.fromInt time)


toTime : Time.Zone -> Posix -> String
toTime timezone time =
    padTime (Time.toHour timezone time)
        ++ ":"
        ++ padTime (Time.toMinute timezone time)
        ++ ":"
        ++ padTime (Time.toSecond timezone time)


toMonth : Time.Month -> String
toMonth month =
    case month of
        Time.Jan ->
            "01"

        Time.Feb ->
            "02"

        Time.Mar ->
            "03"

        Time.Apr ->
            "04"

        Time.May ->
            "05"

        Time.Jun ->
            "06"

        Time.Jul ->
            "07"

        Time.Aug ->
            "08"

        Time.Sep ->
            "09"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


toDate : Time.Zone -> Posix -> String
toDate timezone time =
    String.fromInt (Time.toYear timezone time)
        ++ "-"
        ++ toMonth (Time.toMonth timezone time)
        ++ "-"
        ++ padTime (Time.toDay timezone time)


toDateTime : Time.Zone -> Posix -> String
toDateTime timezone time =
    toDate timezone time ++ " " ++ toTime timezone time


view : Model -> Html Msg
view model =
    div []
        [ case model.details of
            Nothing ->
                div [] []

            Just job ->
                detailsModal job model.groups model.tests
        , case model.error of
            Nothing ->
                div [] []

            Just error ->
                div [ class "notification", class "is-warning" ] [ text error ]
        , h1 [ class "title" ] [ text "Manage Jobs" ]
        , div [ class "is-size-7" ]
            [ text
                ("Last updated " ++ toTime model.timezone model.lastUpdated)
            ]
        , jobsTable model
        , addJob model
        ]



-- HTTP


getJobs : Cmd Msg
getJobs =
    Http.get
        { url = "/job"
        , expect = Http.expectJson GotJobs jobsDecoder
        }


jobsDecoder : Decoder (List Job)
jobsDecoder =
    list
        (Decode.succeed Job
            |> required "id" int
            |> required "created_at" Iso8601.decoder
            |> required "started_at" (nullable Iso8601.decoder)
            |> required "completed_at" (nullable Iso8601.decoder)
            |> required "status" string
            |> required "group_id" int
            |> required "test_id" int
            |> required "output" string
        )


createJob : Job -> Cmd Msg
createJob job =
    Http.post
        { url = "/job"
        , body = Http.jsonBody (jobEncoder job)
        , expect = Http.expectWhatever ReloadJobs
        }


nullableTimestamp : Maybe Posix -> Encode.Value
nullableTimestamp maybePosix =
    case maybePosix of
        Just posix ->
            Iso8601.encode posix

        Nothing ->
            Encode.null


jobEncoder : Job -> Encode.Value
jobEncoder job =
    Encode.object
        [ ( "id", Encode.int job.id )
        , ( "created_at", Iso8601.encode job.created )
        , ( "started_at", nullableTimestamp job.started )
        , ( "completed_at", nullableTimestamp job.completed )
        , ( "status", Encode.string job.status )
        , ( "group_id", Encode.int job.groupID )
        , ( "test_id", Encode.int job.testID )
        , ( "output", Encode.string job.output )
        ]


deleteJob : Int -> Cmd Msg
deleteJob id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/job/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever ReloadJobs
        , timeout = Nothing
        , tracker = Nothing
        }
