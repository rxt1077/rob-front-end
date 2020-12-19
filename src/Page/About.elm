module Page.About exposing (..)

import Browser
import Html exposing (Html, div, text, h3, h4, p, a)
import Html.Attributes exposing (class)



-- MODEL


type alias Model = {}

init : () -> (Model, Cmd Msg)
init _ =
  ( {} , Cmd.none )


-- UPDATE


type Msg
  = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- VIEW

-- provides HTML for the main view
view : Model -> Html Msg
view model =
  div []
    [ div [ class "block" ]
      [ h3 [ class "title"] [ text "About" ]
      , p []
        [ text
        """
        Rob the Builder is a simple testing framework for group projects. It was
        designed to help students see the results of running instructor-designed
        tests on their projects.
        """
        ]
      ]
    , div [ class "block" ]
      [ h4 [ class "title", class "is-4" ] [ text "Student Quick Start" ]
      , p []
        [ text
        """
        Student options can all be accessed from the "Student View" module
        (accessible from the Navbar at the top of your screen).
        """
        ]
      ]
    , div [ class "block" ]
      [ h4 [ class "title", class "is-4" ] [ text "Instructor Quick Start" ]
      , p []
        [ text
        """
        Rob the Builder is comprised of several objects each of which can be
        managed via modules accesible from the Navbar at the top of the screen.
        A description of each object is given below.
        """
        ]
      ]
    ]
