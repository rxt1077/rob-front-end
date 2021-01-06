module Page.ManageGroups exposing (..)

import Browser
import Common
    exposing
        ( bulmaButton
        , bulmaClassButton
        , bulmaTextInput
        , handleError
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , footer
        , h1
        , header
        , i
        , input
        , label
        , p
        , section
        , span
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes
    exposing
        ( class
        , classList
        , placeholder
        , style
        , type_
        , value
        )
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
    exposing
        ( Decoder
        , bool
        , field
        , int
        , list
        , map2
        , map4
        , string
        )
import Json.Encode as Encode
import Platform.Cmd exposing (batch)



-- MODEL


type alias Group =
    { id : Int
    , name : String
    , gitUrl : String
    , members : List String
    }


setGroupName : Group -> String -> Group
setGroupName group name =
    { group | name = name }


setGroupGitUrl : Group -> String -> Group
setGroupGitUrl group gitUrl =
    { group | gitUrl = gitUrl }


setGroupMembers : Group -> List String -> Group
setGroupMembers group members =
    { group | members = members }


type alias User =
    { ucid : String
    , google_name : String
    }


type alias ModalData =
    { group : Group
    , searchText : String
    , buttonText : String
    , title : String
    , onClick : Group -> Msg
    }


type alias Model =
    { groups : List Group
    , users : List User
    , modalData : Maybe ModalData
    , error : Maybe String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { groups = []
      , users = []
      , modalData = Nothing
      , error = Nothing
      }
    , batch [ getUsers, getGroups GotGroups ]
    )



-- UPDATE


type Msg
    = GotGroups (Result Http.Error (List Group))
    | GotUsers (Result Http.Error (List User))
    | AddGroupButton
    | EditGroupButton Group
    | DeleteGroupButton Group
    | CancelEditButton
    | CreateGroup Group
    | UpdateGroup Group
    | ReloadGroups (Result Http.Error ())
    | AddMember String
    | DeleteMember String
    | SearchInput String
    | NameInput String
    | GitInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotGroups result ->
            case result of
                Ok groups ->
                    ( { model | groups = groups, error = Nothing }, Cmd.none )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        GotUsers result ->
            case result of
                Ok users ->
                    ( { model | users = users, error = Nothing }, Cmd.none )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        AddGroupButton ->
            ( { model
                | modalData =
                    Just
                        { group = { id = 0, name = "", gitUrl = "", members = [] }
                        , searchText = ""
                        , buttonText = "Save"
                        , title = "Create a New Group"
                        , onClick = CreateGroup
                        }
              }
            , Cmd.none
            )

        EditGroupButton group ->
            ( { model
                | modalData =
                    Just
                        { group = group
                        , searchText = ""
                        , buttonText = "Update"
                        , title = "Editing Group " ++ String.fromInt group.id
                        , onClick = UpdateGroup
                        }
              }
            , Cmd.none
            )

        DeleteGroupButton group ->
            ( model, deleteGroup group.id )

        CancelEditButton ->
            ( { model | modalData = Nothing }, Cmd.none )

        CreateGroup group ->
            ( { model | modalData = Nothing }, createGroup group )

        UpdateGroup group ->
            ( { model | modalData = Nothing }, updateGroup group )

        ReloadGroups result ->
            case result of
                Ok _ ->
                    ( model, batch [ getUsers, getGroups GotGroups ] )

                Err error ->
                    handleError error (\n -> { model | error = Just n })

        AddMember ucid ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    -- don't allow the same member in a group multiple times
                    if List.member ucid modalData.group.members then
                        ( { model | modalData = Just { modalData | searchText = "" } }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | modalData =
                                Just
                                    { modalData
                                        | group =
                                            setGroupMembers
                                                modalData.group
                                                (ucid :: modalData.group.members)
                                        , searchText = ""
                                    }
                          }
                        , Cmd.none
                        )

        DeleteMember ucid ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    ( { model
                        | modalData =
                            Just
                                { modalData
                                    | group =
                                        setGroupMembers
                                            modalData.group
                                            (List.filter (\u -> u /= ucid) modalData.group.members)
                                }
                      }
                    , Cmd.none
                    )

        SearchInput searchText ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    ( { model | modalData = Just { modalData | searchText = searchText } }
                    , Cmd.none
                    )

        NameInput nameText ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    ( { model
                        | modalData =
                            Just
                                { modalData
                                    | group =
                                        setGroupName modalData.group nameText
                                }
                      }
                    , Cmd.none
                    )

        GitInput gitText ->
            case model.modalData of
                Nothing ->
                    ( model, Cmd.none )

                Just modalData ->
                    ( { model
                        | modalData =
                            Just
                                { modalData
                                    | group =
                                        setGroupGitUrl modalData.group gitText
                                }
                      }
                    , Cmd.none
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW
-- provides HTML for the Edit and Delete buttons at the end of a row


rowControls : Group -> Html Msg
rowControls group =
    div [ class "field", class "is-grouped" ]
        [ bulmaClassButton "Edit" (EditGroupButton group) "is-link"
        , bulmaClassButton "Delete" (DeleteGroupButton group) "is-danger"
        ]


rowItem : Group -> Html Msg
rowItem group =
    tr []
        [ td [] [ text (String.fromInt group.id) ]
        , td [] [ text group.name ]
        , td [] [ text group.gitUrl ]
        , td [] (List.map (\ucid -> div [] [ text ucid ]) group.members)
        , td [] [ rowControls group ]
        ]


groupTable : List Group -> Html Msg
groupTable groups =
    table [ class "table", class "is-hoverable", class "is-fullwidth" ]
        [ thead []
            [ tr []
                [ th [] [ text "ID" ]
                , th [] [ text "Name" ]
                , th [] [ text "Git URL" ]
                , th [] [ text "Members" ]
                , th [] [ text "Actions" ]
                ]
            ]
        , tbody [] (List.map rowItem groups)
        ]



-- provides HTML for an add group button


addGroupButton : Html Msg
addGroupButton =
    div [ class "field", class "is-grouped" ]
        [ bulmaClassButton "Add group" AddGroupButton "is-primary" ]



-- displays a member name with the trash icon next to them


memberIcon : String -> Html Msg
memberIcon ucid =
    p []
        [ span
            [ class "icon"
            , class "is-clickable"
            , class "has-text-danger"
            , onClick (DeleteMember ucid)
            ]
            [ i [ class "fas", class "fa-trash" ] [] ]
        , text ucid
        ]



-- provides HTML for a link within a dropdown menu


dropLink : User -> Html Msg
dropLink user =
    div
        [ class "dropdown-item"
        , class "is-clickable"
        , onClick (AddMember user.ucid)
        ]
        [ text (user.google_name ++ " (" ++ user.ucid ++ ")") ]



-- determines if a User should be displayed in the dropdown list


matchUser : User -> String -> Bool
matchUser user searchText =
    String.contains searchText (String.toLower user.ucid)
        || String.contains searchText (String.toLower user.google_name)



-- filters a User list down to which ones should be shown


filterUsers : List User -> String -> List User
filterUsers users searchText =
    List.filter (\u -> matchUser u (String.toLower searchText)) users



-- provides HTML for a live-updating, searchable dropdown selector of users


autoDrop : List User -> String -> Html Msg
autoDrop users searchText =
    let
        showUsers =
            filterUsers users searchText

        active =
            List.length showUsers <= 5
    in
    div [ classList [ ( "dropdown", True ), ( "is-active", active ) ] ]
        [ div [ class "dropdown-trigger" ]
            [ div [ class "field" ]
                [ p [ class "control", class "is-expanded", class "has-icons-right" ]
                    [ input
                        [ class "input"
                        , type_ "search"
                        , placeholder "Search..."
                        , value searchText
                        , onInput SearchInput
                        ]
                        []
                    , span [ class "icon", class "is-small", class "is-right" ]
                        [ i [ class "fas", class "fa-search" ] [] ]
                    ]
                ]
            ]
        , div [ class "dropdown-menu" ]
            [ div [ class "dropdown-content" ]
                (List.map dropLink showUsers)
            ]
        ]



-- provides HTML for a group edit modal


editModal : ModalData -> List User -> Html Msg
editModal modalData users =
    div [ class "modal", class "is-active" ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
            [ header [ class "modal-card-head" ]
                [ p [ class "modal-card-title" ]
                    [ text modalData.title ]
                ]

            -- the popup dropdown can cause an overflow, so we add a height
            , section [ class "modal-card-body", style "height" "375px" ]
                [ div [ class "columns" ]
                    [ div [ class "column" ]
                        [ bulmaTextInput "Name" NameInput modalData.group.name
                        , div [ class "field" ]
                            (label [ class "label" ] [ text "Members" ]
                                :: List.map memberIcon modalData.group.members
                            )
                        ]
                    , div [ class "column" ]
                        [ bulmaTextInput "Git URL" GitInput modalData.group.gitUrl
                        , div [ class "field" ]
                            [ label [ class "label" ] [ text "Add member" ]
                            , autoDrop users modalData.searchText
                            ]
                        ]
                    ]
                ]
            , footer [ class "modal-card-foot" ]
                [ div [ class "field", class "is-grouped" ]
                    [ bulmaClassButton
                        modalData.buttonText
                        (modalData.onClick modalData.group)
                        "is-success"
                    , bulmaButton "Cancel" CancelEditButton
                    ]
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    div []
        [ case model.modalData of
            Nothing ->
                div [] []

            Just modalData ->
                editModal modalData model.users
        , case model.error of
            Nothing ->
                div [] []

            Just err ->
                div [ class "notification", class "is-warning" ] [ text err ]
        , h1 [ class "title" ] [ text "Manage Groups" ]
        , div [] [ groupTable model.groups ]
        , addGroupButton
        ]



-- HTTP


getGroups : (Result Http.Error (List Group) -> msg) -> Cmd msg
getGroups event =
    Http.get
        { url = "/group"
        , expect = Http.expectJson event groupsDecoder
        }


groupsDecoder : Decoder (List Group)
groupsDecoder =
    list
        (map4 Group
            (field "id" int)
            (field "name" string)
            (field "git_url" string)
            (field "members" (list string))
        )


getUsers : Cmd Msg
getUsers =
    Http.get
        { url = "/user"
        , expect = Http.expectJson GotUsers usersDecoder
        }


usersDecoder : Decoder (List User)
usersDecoder =
    list
        (map2 User
            (field "ucid" string)
            (field "google_name" string)
        )


groupEncoder : Group -> Encode.Value
groupEncoder group =
    Encode.object
        [ ( "id", Encode.int group.id )
        , ( "name", Encode.string group.name )
        , ( "git_url", Encode.string group.gitUrl )
        , ( "members", Encode.list Encode.string group.members )
        ]


createGroup : Group -> Cmd Msg
createGroup group =
    Http.post
        { url = "/group"
        , body = Http.jsonBody (groupEncoder group)
        , expect = Http.expectWhatever ReloadGroups
        }


updateGroup : Group -> Cmd Msg
updateGroup group =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/group/" ++ String.fromInt group.id
        , body = Http.jsonBody (groupEncoder group)
        , expect = Http.expectWhatever ReloadGroups
        , timeout = Nothing
        , tracker = Nothing
        }


deleteGroup : Int -> Cmd Msg
deleteGroup id =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = "/group/" ++ String.fromInt id
        , body = Http.emptyBody
        , expect = Http.expectWhatever ReloadGroups
        , timeout = Nothing
        , tracker = Nothing
        }
