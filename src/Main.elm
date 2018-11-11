module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)


type alias Tag =
    String


type alias Note =
    { text : String
    , tags : List Tag
    }


type alias Model =
    { notes : List Note
    }


type Msg
    = Hello


getDummyNote : String -> Note
getDummyNote text =
    Note text []


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes =
            [ getDummyNote "a"
            , getDummyNote "b"
            , getDummyNote "c"
            ]
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


noteAsHtml : Note -> Html Msg
noteAsHtml note =
    div []
        [ text note.text
        ]


notesAsHtml : List Note -> Html Msg
notesAsHtml notes =
    div []
        (List.map
            noteAsHtml
            notes
        )


view : Model -> Html Msg
view model =
    div []
        [ notesAsHtml model.notes
        ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
