module Common exposing (handleError, bulmaButton, bulmaClassButton,
  bulmaTextInput, emptyToNothing, bulmaClassDisabledButton)

{-| This module contains code for things that are reused across pages in the
creation of the Rob the Builder front end.

# Error Handling
@docs handleError

# View Styling
@docs bulmaButton, bulmaClassButton, bulmaTextInput

# General
@docs emptyToNothing

-}

import Http
import Html exposing (Html, button, p, text, label, div, input)
import Html.Attributes exposing (class, value, type_, disabled)
import Html.Events exposing (onInput, onClick)
import Browser.Navigation as Nav

{-| Handles errors that may pop up in response to HTTP requests. It is assumed
that you have a String in your model that is used to display an error.
Typically this is a Maybe String because you may not always have an active
error. On 401 auth errors, the page is redirected (Nav.load) to "/login".

  --
  case result of
    Ok data ->
      -- do useful things
    Err error ->
      handleError error (\e -> { model | lastError = Just e })

-}
handleError : Http.Error -> (String -> model) -> (model, Cmd msg)
handleError error setter =
  case error of
    Http.BadUrl url ->
      ((setter ("Invalid URL provided: " ++ url)), Cmd.none)
    Http.Timeout ->
      ((setter "Timed out attempting to contact backend API"), Cmd.none)
    Http.NetworkError ->
      ((setter "Network error attempting to contact backend API"), Cmd.none)
    Http.BadStatus num ->
      if num == 401 then
        ((setter "Auth error, redirecting to login page"), Nav.load("/login"))
      else
        ((setter ("HTTP Error " ++ String.fromInt num)), Cmd.none)
    Http.BadBody decoderError ->
      ((setter ("Unable to decode body " ++ decoderError)), Cmd.none)

{-| provides HTML for a Bulma styled button
-}
bulmaButton : String -> msg -> Html msg
bulmaButton bText bOnClick =
  p [ class "control" ]
    [ button [ class "button", onClick bOnClick ] [ text bText ] ]

{-| provides HTML for a Bulma styled button with an additional class
-}
bulmaClassButton : String -> msg -> String -> Html msg
bulmaClassButton bText bOnClick bClass =
  p [ class "control" ]
    [ button [ class "button", class bClass, onClick bOnClick ] [ text bText ] ]

{-| provides HTML for a disabled Bulma styled button with an additional class
-}
bulmaClassDisabledButton : String -> String -> Html msg
bulmaClassDisabledButton bText bClass =
  p [ class "control" ]
    [ button [ class "button", class bClass, disabled True ] [ text bText ] ]

{- provides HTML for a Bulma styled text input
-}
bulmaTextInput : String -> (String -> msg) -> String -> Html msg
bulmaTextInput name event default =
  div [ class "field" ]
    [ label [ class "label" ] [ text name ]
    , div [ class "control" ]
      [ input
        [ class "input"
        , type_ "text"
        , onInput event
        , value default
        ]
        []
      ]
    ]

{- takes a String and returns a Maybe String that is Nothing if the first
string was empty
-}
emptyToNothing : String -> Maybe String
emptyToNothing value =
  case value of
    "" ->
      Nothing
    _ ->
      Just value
