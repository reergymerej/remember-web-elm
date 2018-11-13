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


filterQueryList : Filter -> List Url.Builder.QueryParameter
filterQueryList filter =
    -- For some crazy reason, the service needs 2 $in to work.
    if List.length filter == 0 then
        []

    else
        [ Url.Builder.string "tags[$in]" "" ]
            ++ List.map (\value -> Url.Builder.string "tags[$in]" value) filter


urlForNotes : Int -> Int -> Filter -> String
urlForNotes page pageSize filter =
    let
        basicParams =
            [ Url.Builder.string "$sort[createdAt]" "-1"
            , Url.Builder.string "$limit" (String.fromInt pageSize)
            , Url.Builder.string "$skip" (String.fromInt (page * pageSize))
            ]

        filterParams =
            filterQueryList filter
    in
    Url.Builder.crossOrigin
        "https://jex-forget-me-not.herokuapp.com"
        [ "note"
        ]
        (basicParams
            ++ filterParams
        )


loadNotes : Int -> Int -> Filter -> Cmd Msg
loadNotes page pageSize filter =
    Http.send LoadNotesDone (Http.get (urlForNotes page pageSize filter) notesResponseDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { notes = []
      , loadingState = Loaded
      , page = 0
      , pageSize = 13
      , lastPage = 0
      , filter = []
      }
    , loadNotes 0 8 []
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
                    , loadNotes nextPage nextPageSize model.filter
                    )

        LoadNotes page ->
            ( { model
                | loadingState = Loading
                , page = page
              }
            , loadNotes page model.pageSize model.filter
            )

        LoadNotesDone result ->
            case result of
                Ok notesResponse ->
                    ( { model
                        | loadingState = Loaded
                        , notes = notesResponse.data
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
            let
                nextFilter =
                    addToListIfMissing tag model.filter
            in
            ( { model
                | filter = nextFilter
              }
              -- TODO: Only load if the filter changed.
            , loadNotes model.page model.pageSize nextFilter
            )

        RemoveFilter tag ->
            let
                nextFilter =
                    List.filter (notValue tag) model.filter
            in
            ( { model
                | filter = nextFilter
              }
              -- TODO: Only load if the filter changed.
            , loadNotes model.page model.pageSize nextFilter
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


loadPageButtonText : Int -> Html Msg
loadPageButtonText page =
    text ("load page " ++ String.fromInt page)


pagingView : Model -> Html Msg
pagingView model =
    let
        disabled =
            model.loadingState == Loading

        disableFirst =
            disabled
                || (model.page == 0)

        disablePrevious =
            disabled
                || (model.page < 2)

        disableNext =
            disabled
                || (model.page >= model.lastPage - 1)

        disableLast =
            disabled
                || (model.page == model.lastPage)
    in
    div []
        [ button
            [ onClick (LoadNotes 0)
            , Html.Attributes.disabled disableFirst
            ]
            [ loadPageButtonText 0 ]
        , button
            [ onClick (LoadNotes (model.page - 1))
            , Html.Attributes.disabled disablePrevious
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
            , Html.Attributes.disabled disabled
            ]
            []
        , button
            [ onClick (LoadNotes (model.page + 1))
            , Html.Attributes.disabled disableNext
            ]
            [ loadPageButtonText (model.page + 1) ]
        , button
            [ onClick (LoadNotes model.lastPage)
            , Html.Attributes.disabled disableLast
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
