module Main exposing (main)

import Array
import Browser
import Browser.Events as Events
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Http
import Json.Decode as Decode
import Random


seed0 =
    Random.initialSeed 31415


split : Int -> List a -> List (List a)
split i list =
    case List.take i list of
        [] ->
            []

        listHead ->
            listHead :: split i (List.drop i list)


notEmpty : String -> Bool
notEmpty str =
    not (String.isEmpty str)



--getPoem : Int -> List String -> List String


getPoem index arr =
    let
        chunks =
            Array.fromList <| split 14 arr
    in
    case Array.get index chunks of
        Nothing ->
            List.repeat 14 ""

        Just a ->
            a


getLine : Int -> Int -> String -> String
getLine line index fullText =
    Maybe.withDefault "oops" <| Array.get line <| Array.fromList <| getPoem index <| List.filter notEmpty <| String.lines fullText



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Status
    = Failure
    | Loading
    | Success String


type alias Model =
    { index : Int
    , indexes : List Int
    , status : Status
    , started : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { index = 0, status = Loading, indexes = List.repeat 14 0, started = False }
    , Http.get
        { url = "/src/poemas.txt"
        , expect = Http.expectString GotText
        }
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onKeyDown keyDecoder


keyDecoder : Decode.Decoder Msg
keyDecoder =
    let
        msg =
            KeyboardMsg << toDirection
    in
    Decode.map msg (Decode.field "key" Decode.string)


toDirection : String -> KeyboardMsg
toDirection string =
    case string of
        "ArrowLeft" ->
            ArrowLeft

        "ArrowRight" ->
            ArrowRight

        "ArrowUp" ->
            ArrowUp

        "ArrowDown" ->
            ArrowDown

        "Enter" ->
            Enter

        _ ->
            OtherKey


nextPoemLine model line =
    let
        incr l idx =
            if l == line then
                modBy 10 (idx + 1)

            else
                idx

        idxs =
            List.indexedMap incr model.indexes
    in
    ( { model | indexes = idxs }, Cmd.none )


prevPoemLine model line =
    let
        decr l idx =
            if l == line then
                modBy 10 (idx - 1)

            else
                idx

        idxs =
            List.indexedMap decr model.indexes
    in
    ( { model | indexes = idxs }, Cmd.none )



-- UPDATE


type KeyboardMsg
    = ArrowLeft
    | ArrowRight
    | ArrowUp
    | ArrowDown
    | Enter
    | OtherKey


type Msg
    = Increment Int
    | Decrement Int
    | GotText (Result Http.Error String)
    | KeyboardMsg KeyboardMsg
    | RandomIndexes (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( { model | status = Success fullText }, Cmd.none )

                Err _ ->
                    ( { model | status = Failure }, Cmd.none )

        Increment line ->
            nextPoemLine model line

        Decrement line ->
            prevPoemLine model line

        RandomIndexes idxs ->
            ( { model | indexes = idxs }, Cmd.none )

        KeyboardMsg key ->
            case key of
                ArrowLeft ->
                    prevPoemLine model model.index

                ArrowRight ->
                    nextPoemLine model model.index

                ArrowUp ->
                    ( { model | index = modBy 14 (model.index - 1) }, Cmd.none )

                ArrowDown ->
                    ( { model | index = modBy 14 (model.index + 1) }, Cmd.none )

                Enter ->
                    ( { model | started = True }, Random.generate RandomIndexes (Random.list 14 (Random.int 0 9)) )

                OtherKey ->
                    ( model, Cmd.none )


linePadding line =
    paddingEach
        { top = 0
        , right = 0
        , bottom =
            if List.member line [ 3, 7, 10 ] then
                20

            else
                0
        , left = 0
        }


renderLine line idx curr fulltext =
    let
        fc =
            if line == curr then
                Font.bold

            else
                Font.regular
    in
    row [ width fill, linePadding line ]
        [ el
            [ Font.color (rgb 0 0 0)
            , fc
            , width fill
            ]
            (text (getLine line idx fulltext))
        ]


mapLines fullText index indexes line =
    let
        idx =
            Maybe.withDefault 0 <| Array.get line <| Array.fromList <| indexes
    in
    renderLine line idx index fullText


renderIntro =
    column
        [ width fill
        , centerY
        , Font.center
        , Font.size 14
        ]
        [ el [ width fill, padding 30, Font.size 40 ] <| text "Algo Leo"
        , el [ width fill, padding 5 ] <| text "Generador Algorítimo de"
        , el [ width fill ] <| text "Sonetos Alejandrinos para Leer"
        , el [ width fill, padding 20, Font.size 14 ] <| text "Presiona ENTER para comenzar."
        , el [ width fill, padding 15, Font.size 40 ] <| text "↵"
        , el [ width fill, padding 5, Font.color <| rgb 0.25 0.25 0.25, Font.center ] <|
            text "↵ genera un nuevo soneto"
        , el [ width fill, padding 5, Font.color <| rgb 0.25 0.25 0.25, Font.center ] <|
            text "↑ ↓ → ← cambia los versos."
        ]


renderPoems model fullText =
    column
        [ width fill
        , centerY
        , spacing 8
        , Font.center
        ]
    <|
        List.map
            (mapLines fullText model.index model.indexes)
            (List.range 0 13)



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.image "bg.jpg"
        , width fill
        , height fill
        , Font.family
            [ Font.external
                { -- url = "https://fonts.googleapis.com/css?family=Homemade+Apple&display=swap"
                  url = "https://fonts.googleapis.com/css?family=Beth+Ellen&display=swap"

                -- url = "https://fonts.googleapis.com/css?family=Nothing+You+Could+Do&display=swap"
                -- , name = "Homemade Apple"
                , name = "Beth Ellen"

                -- , name = "Nothing You Could Do"
                }
            , Font.sansSerif
            ]
        ]
    <|
        case model.status of
            Failure ->
                el [] <| text "Error."

            Loading ->
                el [] <| text "Loading..."

            Success fullText ->
                if model.started then
                    renderPoems model fullText

                else
                    renderIntro
