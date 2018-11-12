module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, pre, text, ul)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
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
    , pageSize : Int
    , lastPage : Int
    , canLoadMore : Bool
    }


type Msg
    = LoadNotes Int
    | LoadNotesDone (Result Http.Error NotesResponse)
    | SetPageSize (Maybe Int)


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


urlForPage : Int -> Int -> String
urlForPage page pageSize =
    Url.Builder.crossOrigin
        "https://jex-forget-me-not.herokuapp.com"
        [ "note"
        ]
        [ Url.Builder.string "$sort[createdAt]" "-1"
        , Url.Builder.string "$limit" (String.fromInt pageSize)
        , Url.Builder.string "$skip" (String.fromInt (page * pageSize))
        ]


loadNotes : Int -> Int -> Cmd Msg
loadNotes page pageSize =
    Http.send LoadNotesDone (Http.get (urlForPage page pageSize) notesResponseDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , loadingState = Loaded
      , page = 0
      , pageSize = 8
      , lastPage = 0
      , canLoadMore = True
      }
    , loadNotes 0 8
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
        { total, skip, limit } =
            response
    in
    total > (skip + limit)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadNotes page ->
            ( { model
                | loadingState = Loading
                , page = page
              }
            , loadNotes page model.pageSize
            )

        LoadNotesDone result ->
            case result of
                Ok notesResponse ->
                    ( { model
                        | loadingState = Loaded
                        , notes = notesResponse.data
                        , canLoadMore = hasMorePagesToLoad notesResponse
                        , lastPage = notesResponse.total // model.pageSize
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

        SetPageSize size ->
            case size of
                Nothing ->
                    ( model, Cmd.none )

                Just pageSize ->
                    ( { model | pageSize = pageSize }
                    , Cmd.none
                    )


tagAsHtml : Tag -> Html Msg
tagAsHtml tag =
    li [] [ text tag ]


noteAsHtml : Note -> Html Msg
noteAsHtml note =
    div []
        [ div []
            [ text note.text ]
        , ul [] (List.map tagAsHtml note.tags)
        ]


canLoadMore : Model -> Bool
canLoadMore model =
    not
        (model.loadingState
            == Loading
            || model.canLoadMore
        )


loadPageButtonText : Int -> Html Msg
loadPageButtonText page =
    text ("load page " ++ String.fromInt page)


pagingView : Model -> Html Msg
pagingView model =
    div []
        [ button
            [ onClick (LoadNotes 0)
            , Html.Attributes.disabled (model.page == 0)
            ]
            [ loadPageButtonText 0 ]
        , button
            [ onClick (LoadNotes (model.page - 1))
            , Html.Attributes.disabled (model.page < 2)
            ]
            [ loadPageButtonText (model.page - 1) ]
        , input
            [ Html.Attributes.placeholder "page size"
            , Html.Attributes.type_ "number"
            , Html.Attributes.value (String.fromInt model.pageSize)
            , Html.Attributes.min "1"
            , Html.Attributes.max "100"
            , Html.Attributes.step "10"
            , onInput (\value -> SetPageSize (String.toInt value))
            ]
            []
        , button
            [ onClick (LoadNotes (model.page + 1))
            , Html.Attributes.disabled (canLoadMore model && model.page >= model.lastPage - 1)
            ]
            [ loadPageButtonText (model.page + 1) ]
        , button
            [ onClick (LoadNotes model.lastPage)
            , Html.Attributes.disabled (model.page == model.lastPage)
            ]
            [ loadPageButtonText model.lastPage ]
        ]


loadedView : List Note -> Html Msg
loadedView notes =
    div []
        (List.map
            noteAsHtml
            notes
        )


loadingView : Html Msg
loadingView =
    div [] [ text "loading..." ]


loadFailedView : String -> Html Msg
loadFailedView error =
    pre [] [ text error ]


view : Model -> Html Msg
view model =
    div []
        [ pagingView model
        , case model.loadingState of
            Failed error ->
                loadFailedView error

            Loading ->
                loadingView

            Loaded ->
                loadedView model.notes
        ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
