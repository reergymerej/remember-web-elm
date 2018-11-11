module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Url.Builder


type alias Tag =
    String


type alias Note =
    { text : String
    , tags : List Tag
    }


type LoadingState
    = Loaded
    | Loading
    | Failed String


type alias Model =
    { notes : List Note
    , loadingState : LoadingState
    }


type Msg
    = LoadNotes
    | LoadNotesDone (Result Http.Error (List Note))


textDecoder : D.Decoder String
textDecoder =
    D.field "text" D.string


tagsDecoder : D.Decoder (List String)
tagsDecoder =
    D.field "tags" (D.list D.string)


noteDecoder : D.Decoder Note
noteDecoder =
    D.map2 Note
        textDecoder
        tagsDecoder


dataDecoder : D.Decoder (List Note)
dataDecoder =
    D.field "data" (D.list noteDecoder)


loadNotes : Cmd Msg
loadNotes =
    let
        url =
            Url.Builder.crossOrigin
                "https://jex-forget-me-not.herokuapp.com"
                [ "note"
                ]
                [ Url.Builder.string "$sort[createdAt]" "-1"
                , Url.Builder.string "$skip" "0"
                ]
    in
    Http.send LoadNotesDone (Http.get url dataDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , loadingState = Loaded
      }
    , loadNotes
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


getStringFromHttpError : Http.Error -> String
getStringFromHttpError error =
    case error of
        Http.BadUrl msg ->
            msg

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus _ ->
            "bad status"

        Http.BadPayload msg _ ->
            msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadNotes ->
            ( { model | loadingState = Loading }
            , loadNotes
            )

        LoadNotesDone result ->
            case result of
                Ok notes ->
                    ( { model
                        | loadingState = Loaded
                        , notes = notes
                      }
                    , Cmd.none
                    )

                Err error ->
                    ( { model
                        | loadingState =
                            Failed
                                (getStringFromHttpError
                                    error
                                )
                      }
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
        [ div []
            [ button [ onClick LoadNotes ] [ text "LoadNotes" ]
            ]
        , div []
            (case model.loadingState of
                Loaded ->
                    [ notesAsHtml model.notes
                    ]

                Loading ->
                    [ text "loading..." ]

                Failed error ->
                    [ text error ]
            )
        ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
