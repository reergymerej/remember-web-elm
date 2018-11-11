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


type alias NotesResponse =
    { data : List Note
    , limit : Int
    , skip : Int
    , total : Int
    }


type LoadingState
    = Loaded
    | Loading
    | Failed String


type alias Model =
    { notes : List Note
    , loadingState : LoadingState
    , page : Int
    , canLoadMore : Bool
    }


type Msg
    = LoadNotes
    | LoadNotesDone (Result Http.Error NotesResponse)


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


notesResponseDecoder : D.Decoder NotesResponse
notesResponseDecoder =
    D.map4 NotesResponse
        (D.field "data" (D.list noteDecoder))
        (D.field "limit" D.int)
        (D.field "skip" D.int)
        (D.field "total" D.int)


loadNotes : Int -> Cmd Msg
loadNotes page =
    let
        pageSize =
            10

        url =
            Url.Builder.crossOrigin
                "https://jex-forget-me-not.herokuapp.com"
                [ "note"
                ]
                [ Url.Builder.string "$sort[createdAt]" "-1"
                , Url.Builder.string "$skip" (String.fromInt (page * pageSize))
                ]
    in
    Http.send LoadNotesDone (Http.get url notesResponseDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , loadingState = Loaded
      , page = 0
      , canLoadMore = True
      }
    , loadNotes 0
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


hasMorePagesToLoad : NotesResponse -> Bool
hasMorePagesToLoad response =
    let
        { total, skip, data } =
            response
    in
    total - (skip + List.length data) > 0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadNotes ->
            ( { model
                | loadingState = Loading
                , page = model.page + 1
              }
            , loadNotes (model.page + 1)
            )

        LoadNotesDone result ->
            case result of
                Ok notesResponse ->
                    ( { model
                        | loadingState = Loaded
                        , notes = notesResponse.data
                        , canLoadMore = hasMorePagesToLoad notesResponse
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


canLoadMore : Model -> Bool
canLoadMore model =
    not
        (model.loadingState
            == Loading
            || model.canLoadMore
        )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ button
                [ onClick LoadNotes
                , Html.Attributes.disabled (canLoadMore model)
                ]
                [ text
                    ("load page "
                        ++ String.fromInt
                            (model.page + 1)
                    )
                ]
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
