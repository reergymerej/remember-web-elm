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


type alias Filter =
    List Tag


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
    , filter : Filter
    }


type Msg
    = LoadNotes Int
    | LoadNotesDone (Result Http.Error NotesResponse)
    | SetPageSize (Maybe Int)
    | AddFilter Tag
    | RemoveFilter Tag


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
      , pageSize = 13
      , lastPage = 0
      , canLoadMore = True
      , filter = []
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


maxPageSize =
    50


minPageSize =
    1


constrainInt : Int -> Int -> Int -> Int
constrainInt min max value =
    if value > max then
        max
    else if value < min then
        min
    else
        value


validPageSize : Int -> Int
validPageSize pageSize =
    constrainInt minPageSize maxPageSize pageSize


pagedLocation : Int -> Int -> Int
pagedLocation pageSize targetIndex =
    targetIndex // pageSize


addToListIfMissing : a -> List a -> List a
addToListIfMissing a list =
    if List.member a list then
        list
    else
        list ++ [ a ]


notValue : a -> a -> Bool
notValue left right =
    left /= right


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPageSize size ->
            case size of
                Nothing ->
                    ( model, Cmd.none )

                Just pageSize ->
                    let
                        nextPageSize =
                            validPageSize pageSize

                        indexOfFirstRecord =
                            model.pageSize * model.page

                        nextPage =
                            pagedLocation nextPageSize indexOfFirstRecord
                    in
                    ( { model
                        | pageSize = nextPageSize
                        , page = nextPage
                      }
                    , loadNotes nextPage nextPageSize
                    )

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
                        , lastPage = pagedLocation model.pageSize (notesResponse.total - 1)
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

        AddFilter tag ->
            ( { model
                | filter = addToListIfMissing tag model.filter
              }
            , Cmd.none
            )

        RemoveFilter tag ->
            ( { model
                | filter = List.filter (notValue tag) model.filter
              }
            , Cmd.none
            )


clickableTag : Msg -> Tag -> Html Msg
clickableTag msg tag =
    li [ onClick msg ] [ text tag ]


addableTag : Tag -> Html Msg
addableTag tag =
    clickableTag (AddFilter tag) tag


removeableTag : Tag -> Html Msg
removeableTag tag =
    clickableTag (RemoveFilter tag) tag


noteAsHtml : Note -> Html Msg
noteAsHtml note =
    div []
        [ div []
            [ text note.text ]
        , ul [] (List.map addableTag note.tags)
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
            , Html.Attributes.min (String.fromInt minPageSize)
            , Html.Attributes.max (String.fromInt maxPageSize)
            , Html.Attributes.step "1"
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


viewLoaded : List Note -> Html Msg
viewLoaded notes =
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


viewFilter : Filter -> Html Msg
viewFilter filter =
    div []
        [ div [] [ text "Filter" ]
        , ul [] (List.map removeableTag filter)
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewFilter model.filter
        , pagingView model
        , case model.loadingState of
            Failed error ->
                loadFailedView error

            Loading ->
                loadingView

            Loaded ->
                viewLoaded model.notes
        ]


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
