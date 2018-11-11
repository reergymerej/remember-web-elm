module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)


type alias Model =
    { greeting : String }


type Msg
    = Hello


init : () -> ( Model, Cmd Msg )
init _ =
    ( { greeting = "Hello"
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model
    , Cmd.none
    )


view : Model -> Html Msg
view model =
    div [] [ text model.greeting ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
