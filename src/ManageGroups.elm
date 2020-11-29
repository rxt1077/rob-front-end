module ManageGroups exposing (..)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, Attribute, div, table, tr, th, td, p, a, text,
  input, button, label, thead, tbody, header, footer, section, i, span, h1)
import Html.Attributes exposing (class, classList, value, type_, placeholder)
import Html.Events exposing (onInput, onClick)
import Http
import Json.Decode exposing (Decoder, field, string, int, list, bool, map2, map4)
import Json.Encode as Encode
import Debug exposing (log)
import Platform.Cmd exposing (batch)


-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions 
    , view = view
    }


-- MODEL


type alias Group =
  { id : Int
  , name : String
  , gitUrl : String
  , members : List String
  }

type alias User = 
  { ucid : String
  , google_name : String
  }

-- Custom type so that modal is either active with data or inactive
type EditModal
  = Active
  { group : Group
  , searchText : String
  , buttonText : String
  , title : String
  , onClick : Group -> Msg
  }
  | Inactive

type alias Model =
  { groups : List Group
  , users : List User
  , editModal : EditModal
  , notification : String
  }

init : () -> (Model, Cmd Msg)
init _ =
  ({ groups = []
  , users = []
  , editModal = Inactive
  , notification = ""
  }, batch [getUsers, getGroups]) 


-- UPDATE

-- sets error messages for the notification DIV and automatically redirects
-- to '/login' if an auth error (401) is recieved
handleErrors : Model -> Http.Error -> (Model, Cmd Msg)
handleErrors model error =
  case error of
    Http.BadUrl url ->
      ({ model | notification = "Invalid URL provided: " ++ url }, Cmd.none)
    Http.Timeout ->
      ({ model | notification = "Timed out attempting to contact backend API" },
        Cmd.none)
    Http.NetworkError ->
      ({ model | notification = "Network error attempting to contact backend API" },
        Cmd.none)
    Http.BadStatus num ->
      if num == 401 then
        ({ model | notification = "Auth error, redirecting to login page" },
          Nav.load("/login"))
      else
        ({ model | notification = "HTTP Error " ++ String.fromInt num }, Cmd.none)
    Http.BadBody decoderError ->
      ({ model | notification = "Unable to decode body " ++ decoderError }, Cmd.none)

type Msg
  = GotGroups (Result Http.Error (List Group))
  | GotUsers (Result Http.Error (List User))
  | AddGroupButton
  | EditGroupButton Group
  | DeleteGroupButton Int
  | CancelEditButton
  | CreateGroup Group
  | UpdateGroup Group
  | ReloadGroups (Result Http.Error ())
  | AddMember String
  | SearchInput String
  | NameInput String
  | GitInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotGroups result ->
      case result of
        Ok groups ->
          ({ model | groups = groups, notification = "" }, Cmd.none)
        Err error ->
          handleErrors model error
    GotUsers result ->
      case result of
        Ok users ->
          ({ model | users = users, notification = "" }, Cmd.none)
        Err error ->
          handleErrors model error
    AddGroupButton ->
      ({ model | editModal =
        Active
        { group = { id = 0, name = "", gitUrl = "", members = [] }
        , searchText = ""
        , buttonText = "Create group"
        , title = "Adding Group"
        , onClick = CreateGroup 
        }
      }, Cmd.none)
    EditGroupButton group ->
      ({ model | editModal =
        Active
        { group = group
        , searchText = ""
        , buttonText = "Save changes"
        , title = "Editing Group " ++ (String.fromInt group.id)
        , onClick = UpdateGroup
        }
      }, Cmd.none)
    DeleteGroupButton id ->
      (model, deleteGroup id)
    CancelEditButton ->
      ({ model | editModal = Inactive }, Cmd.none)
    CreateGroup group ->
      ({ model | editModal = Inactive }, createGroup group)
    UpdateGroup group ->
      ({ model | editModal = Inactive }, updateGroup group)
    ReloadGroups result ->
      case result of
        Ok _ ->
          (model, batch [getUsers, getGroups])
        Err error ->
          handleErrors model error
    AddMember ucid ->
      case model.editModal of
        Inactive ->
          -- you should really never get here
          (model, Cmd.none)
        Active modalData  ->         
          -- don't allow the same member in a group multiple times
          if List.member ucid modalData.group.members then
            ({ model | editModal = Active { modalData | searchText = "" }} , Cmd.none)
          else
            let
              oldGroup =
                modalData.group
              newGroup =
                { oldGroup | members = ucid::oldGroup.members }
            in
              ({ model | editModal =
                Active { modalData | group = newGroup, searchText = "" }
              }, Cmd.none)
    SearchInput searchText ->
      case model.editModal of
        Inactive ->
          (model, Cmd.none)
        Active modalData ->
          ({ model | editModal =
            Active { modalData | searchText = searchText }}, Cmd.none)
    NameInput nameText ->
      case model.editModal of
        Inactive ->
          (model, Cmd.none)
        Active modalData ->
          let oldGroup =
                modalData.group
              newGroup =
                { oldGroup | name = nameText }
          in
            ({ model | editModal =
              Active { modalData | group = newGroup }}, Cmd.none)
    GitInput gitText ->
      case model.editModal of
        Inactive ->
          (model, Cmd.none)
        Active modalData ->
          let oldGroup =
                modalData.group
              newGroup =
                { oldGroup | gitUrl = gitText }
          in
            ({ model | editModal =
              Active { modalData | group = newGroup }}, Cmd.none)

  
-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none


-- VIEW

-- provides HTML for the Edit and Delete buttons at the end of a row
rowControls : Group -> Html Msg
rowControls group =
  div [ class "field", class "is-grouped" ]
    [ p [ class "control" ]
      [ a [ class "button", class "is-link", onClick (EditGroupButton group) ]
        [ text "Edit" ]
      ]
    , p [ class "control" ]
      [ a [ class "button", class "is-danger", onClick (DeleteGroupButton group.id) ]
        [ text "Delete" ]
      ]
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
  table [class "table", class "is-hoverable", class "is-fullwidth"]
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
addGroupButton  =
  div [ class "field", class "is-grouped" ]
    [ p [ class "control" ]
      [ a [ class "button", class "is-primary", onClick AddGroupButton ]
        [ text "Add group" ]
      ]
    ]

memberIcon : String -> Html Msg
memberIcon name =
  p []
  [ a []
    [ span [ class "icon", class "has-text-danger" ]
      [ i [ class "fas", class "fa-trash" ] [] ]
    ]
  , text name
  ]

-- provides HTML for a link within a dropdown menu
dropLink : User  -> Html Msg
dropLink user =
  a [ class "dropdown-item", onClick (AddMember user.ucid) ]
    [ text (user.google_name ++ " (" ++ user.ucid ++ ")") ]

-- determines if a User should be displayed in the dropdown list
matchUser : User -> String -> Bool
matchUser user searchText =
  (String.contains searchText (String.toLower user.ucid))
  || (String.contains searchText (String.toLower user.google_name))

-- filters a User list down to which ones should be shown
filterUsers : List User -> String -> List User
filterUsers users searchText =
  List.filter (\u -> (matchUser u (String.toLower searchText))) users

-- provides HTML for a live-updating, searchable dropdown selector of users
autoDrop : List User -> String -> Html Msg
autoDrop users searchText =
  let
    showUsers =
      filterUsers (log "autoDrop: users" users) (log "autoDrop: searchText" searchText)
    active =
      (List.length showUsers <= 5)
  in
    div [ classList [ ("dropdown", True), ("is-active", active) ] ]
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
            , span [ class "icon", class "is-small", class "is-right"]
              [ i [ class "fas", class "fa-search" ] []]
            ]
          ]
        ]
      , div [ class "dropdown-menu" ]
        [ div [ class "dropdown-content" ]
          (List.map dropLink showUsers)
        ]
      ]

-- provides HTML for a group edit modal
editModal : Model -> Html Msg
editModal model =
  case model.editModal of
    Inactive ->
      div [] []
    Active modalData ->
      div [ class "modal", class "is-active" ]
        [ div [ class "modal-background" ] []
        , div [ class "modal-card" ]
          [ header [ class "modal-card-head" ]
            [ p [ class "modal-card-title" ]
              [ text modalData.title ]
            ]
          , section [ class "modal-card-body" ]
              [ div [ class "field" ]
                [ label [ class "label" ] [ text "Name"]
                , div [ class "control" ]
                  [ input
                    [ class "input"
                    , type_ "text"
                    , onInput NameInput
                    , value modalData.group.name
                    ]
                    []
                  ]
                ]
              , div [ class "field" ]
                [ label [ class "label" ] [ text "Git URL"]
                , div [ class "control" ]
                  [ input
                    [ class "input"
                    , type_ "text"
                    , onInput GitInput
                    , value modalData.group.gitUrl
                    ]
                    []
                  ]
                ]
              , div [ class "field" ]
                ((label [ class "label" ] [ text "Members" ])
                ::(List.map memberIcon modalData.group.members))
              , div [ class "field" ]
                [ label [ class "label" ] [ text "Add member" ]
                , autoDrop model.users modalData.searchText
                ]
              ]
          , footer [ class "modal-card-foot" ]
            [ button
              [ class "button"
              , class "is-success"
              , onClick (modalData.onClick modalData.group)
              ]
              [ text modalData.buttonText ]
            , button
              [ class "button"
              , onClick CancelEditButton
              ]
              [ text "Cancel" ]
            ]
          ]
        ]

-- provides HTML for an error notification 
notification : String -> Html Msg
notification message =
  if message == "" then
    div [] []
  else
    div [ class "notification", class "is-warning" ] [ text message ]

view : Model -> Html Msg
view model =
  div []
    [ editModal model
    , notification model.notification
    , h1 [ class "title" ] [ text "Manage Groups" ]
    , div [] [ groupTable model.groups ]
    , addGroupButton
    ]


-- HTTP


getGroups : Cmd Msg
getGroups =
  Http.get
    { url = "/group"
    , expect = Http.expectJson GotGroups groupsDecoder
    }

groupsDecoder : Decoder (List Group)
groupsDecoder =
  list (map4 Group
    (field "id" int)
    (field "name" string)
    (field "git_url" string)
    (field "members" (list string)))

getUsers : Cmd Msg
getUsers =
  Http.get
    { url = "/user"
    , expect = Http.expectJson GotUsers usersDecoder
    }

usersDecoder : Decoder (List User)
usersDecoder =
  list (map2 User
    (field "ucid" string)
    (field "google_name" string))

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
    , url = "/group"
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
