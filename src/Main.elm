module Main exposing (Model, init, main, update, view)

import Browser
import Html exposing (Html, button, div, input, li, pre, text, ul)
import Html.Attributes
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
import Url.Builder


type alias Tag =
    String


type alias Filter =
    List Tag


type alias Note =
    { text : String
    , tags : List Tag
    }


type alias NewNote =
    String


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


type SavingNoteState
    = DoneSavingNote
    | SavingNote
    | FailedSavingNote String


type alias Model =
    { notes : List Note
    , loadingState : LoadingState
    , page : Int
    , pageSize : Int
    , lastPage : Int
    , filter : Filter
    , newNote : NewNote
    , addingNewNote : Bool
    , savingNoteState : SavingNoteState
    }


type Msg
    = LoadNotes Int
    | LoadNotesDone (Result Http.Error NotesResponse)
    | SetPageSize (Maybe Int)
    | AddFilter Tag
    | RemoveFilter Tag
    | ToggleAddNote Bool
    | ChangeNewNote String
    | SaveNewNote
    | SaveNoteDone (Result Http.Error Note)


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


saveNoteResponseDecoder : D.Decoder Note
saveNoteResponseDecoder =
    noteDecoder


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
    Http.send LoadNotesDone
        (Http.get
            (urlForNotes page pageSize filter)
            notesResponseDecoder
        )


saveNote : NewNote -> Cmd Msg
saveNote newNote =
    let
        url =
            Url.Builder.crossOrigin
                "https://jex-forget-me-not.herokuapp.com"
                [ "note" ]
                []

        encodedBody =
            E.object [ ( "text", E.string newNote ) ]

        body =
            Http.jsonBody encodedBody
    in
    Http.send SaveNoteDone
        (Http.post url body noteDecoder)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        pageSize =
            13

        page =
            0
    in
    ( { notes = []
      , loadingState = Loaded
      , page = page
      , pageSize = pageSize
      , lastPage = 0
      , filter = []
      , newNote = ""
      , addingNewNote = True
      , savingNoteState = DoneSavingNote
      }
    , loadNotes page pageSize []
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


commandAfterFilterChange : Filter -> Filter -> Int -> Cmd Msg
commandAfterFilterChange nextFilter filter pageSize =
    if filter == nextFilter then
        Cmd.none

    else
        loadNotes 0 pageSize nextFilter


commandAfterFilter : Filter -> Model -> Cmd Msg
commandAfterFilter filter model =
    commandAfterFilterChange filter model.filter model.pageSize


modelAfterFilter : Filter -> Model -> Model
modelAfterFilter nextFilter model =
    { model
        | filter = nextFilter
        , page = 0
    }


tupleAfterFilter : Model -> Filter -> ( Model, Cmd Msg )
tupleAfterFilter model filter =
    ( modelAfterFilter filter model
    , commandAfterFilter filter model
    )


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
                        | loadingState = Failed (getStringFromHttpError error)
                      }
                    , Cmd.none
                    )

        AddFilter tag ->
            tupleAfterFilter model
                (addToListIfMissing tag model.filter)

        RemoveFilter tag ->
            tupleAfterFilter model
                (List.filter (notValue tag) model.filter)

        ToggleAddNote addingNewNote ->
            ( { model | addingNewNote = addingNewNote }, Cmd.none )

        ChangeNewNote newNote ->
            ( { model | newNote = newNote }, Cmd.none )

        SaveNewNote ->
            let
                newNote =
                    String.trim model.newNote
            in
            ( { model
                | newNote = newNote
                , savingNoteState = SavingNote
              }
            , saveNote newNote
            )

        SaveNoteDone result ->
            case result of
                Err error ->
                    ( { model
                        | savingNoteState = FailedSavingNote (getStringFromHttpError error)
                      }
                    , Cmd.none
                    )

                Ok note ->
                    ( { model
                        | savingNoteState = DoneSavingNote
                        , notes = note :: model.notes
                        , newNote = ""
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


loadPageButtonText : Int -> Html Msg
loadPageButtonText page =
    text ("load page " ++ String.fromInt page)


type alias PageViewStates =
    { first : Bool
    , prev : Bool
    , size : Bool
    , next : Bool
    , last : Bool
    }


pageViewStates : Bool -> Int -> Int -> PageViewStates
pageViewStates disabled page lastPage =
    { first =
        disabled
            || (page == 0)
    , prev =
        disabled
            || (page < 2)
    , size = disabled
    , next =
        disabled
            || (page >= lastPage - 1)
    , last =
        disabled
            || (page == lastPage)
    }


viewPaging : Model -> Html Msg
viewPaging model =
    let
        { page, lastPage } =
            model

        states =
            pageViewStates (model.loadingState == Loading) page lastPage
    in
    div []
        [ button
            [ onClick (LoadNotes 0)
            , Html.Attributes.disabled states.first
            ]
            [ loadPageButtonText 0 ]
        , button
            [ onClick (LoadNotes (page - 1))
            , Html.Attributes.disabled states.prev
            ]
            [ loadPageButtonText (page - 1) ]
        , input
            [ Html.Attributes.placeholder "page size"
            , Html.Attributes.type_ "number"
            , Html.Attributes.value (String.fromInt model.pageSize)
            , Html.Attributes.min (String.fromInt minPageSize)
            , Html.Attributes.max (String.fromInt maxPageSize)
            , Html.Attributes.step "1"
            , onInput (\value -> SetPageSize (String.toInt value))
            , Html.Attributes.disabled states.size
            ]
            []
        , button
            [ onClick (LoadNotes (page + 1))
            , Html.Attributes.disabled states.next
            ]
            [ loadPageButtonText (page + 1) ]
        , button
            [ onClick (LoadNotes lastPage)
            , Html.Attributes.disabled states.last
            ]
            [ loadPageButtonText lastPage ]
        ]


viewLoaded : List Note -> Html Msg
viewLoaded notes =
    div []
        (List.map
            noteAsHtml
            notes
        )


viewLoading : Html Msg
viewLoading =
    div [] [ text "loading..." ]


viewLoadFailed : String -> Html Msg
viewLoadFailed error =
    pre [] [ text error ]


viewFilter : Filter -> Html Msg
viewFilter filter =
    div []
        [ div [] [ text "Filter" ]
        , ul [] (List.map removeableTag filter)
        ]


noteIsValid : Model -> Bool
noteIsValid model =
    String.trim model.newNote == ""


viewAddingNote : Model -> Html Msg
viewAddingNote model =
    case model.savingNoteState of
        DoneSavingNote ->
            div []
                [ input
                    [ Html.Attributes.placeholder "Add a Note"
                    , Html.Attributes.value model.newNote
                    , onInput ChangeNewNote
                    ]
                    []
                , button [ onClick (ToggleAddNote False) ] [ text "Cancel" ]
                , button
                    [ Html.Attributes.disabled (noteIsValid model == True)
                    , onClick SaveNewNote
                    ]
                    [ text "Save" ]
                ]

        SavingNote ->
            div []
                [ input
                    [ Html.Attributes.disabled True
                    , onInput ChangeNewNote
                    ]
                    []
                , button [ onClick (ToggleAddNote False) ] [ text "Cancel" ]
                , button
                    [ Html.Attributes.disabled True ]
                    [ text "Saving..." ]
                ]

        FailedSavingNote error ->
            div []
                [ input
                    [ Html.Attributes.placeholder "Add a Note"
                    , onInput ChangeNewNote
                    ]
                    []
                , button [ onClick (ToggleAddNote False) ] [ text "Cancel" ]
                , button
                    [ Html.Attributes.disabled (noteIsValid model == True)
                    , onClick SaveNewNote
                    ]
                    [ text "Try Again" ]
                , div [] [ text error ]
                ]


viewTools : Model -> Html Msg
viewTools model =
    div []
        [ button [ onClick (ToggleAddNote (not model.addingNewNote)) ] [ text "toggle" ]
        , if model.addingNewNote then
            viewAddingNote model

          else
            viewFilter model.filter
        ]


view : Model -> Html Msg
view model =
    div []
        [ viewTools model
        , viewPaging model
        , case model.loadingState of
            Failed error ->
                viewLoadFailed error

            Loading ->
                viewLoading

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
