module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as D


type alias Tag =
    String


type alias Note =
    { text : String
    , tags : List Tag
    }


type LoadingState
    = Loaded
    | Loading
    | Failed


type alias Model =
    { notes : List Note
    , loadingState : LoadingState
    }


type Msg
    = LoadNotes
    | LoadNotesDone
    | LoadNotesFailed


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


decodeNote : String -> Note
decodeNote encoded =
    case D.decodeString noteDecoder encoded of
        Err error ->
            Note (D.errorToString error) []

        Ok note ->
            note


json =
    """{"text":"a dummy note","tags":["tag1","tag2"]}"""


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes =
            [ decodeNote json
            ]
      , loadingState = Loaded
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadNotes ->
            ( { model | loadingState = Loading }
            , Cmd.none
            )

        LoadNotesDone ->
            ( { model | loadingState = Loaded }
            , Cmd.none
            )

        LoadNotesFailed ->
            ( { model | loadingState = Failed }
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
            , button [ onClick LoadNotesDone ] [ text "LoadNotesDone" ]
            , button [ onClick LoadNotesFailed ] [ text "LoadNotesFailed" ]
            ]
        , div []
            (case model.loadingState of
                Loaded ->
                    [ notesAsHtml model.notes
                    ]

                Loading ->
                    [ text "loading..." ]

                Failed ->
                    [ text "failed" ]
            )
        ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
