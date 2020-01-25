module Main exposing (main)

import Array
import Browser
import Browser.Dom exposing (getViewport)
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
import Task


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
    , width : Int
    , height : Int
    , device : Device
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { index = 0
      , status = Loading
      , indexes = List.repeat 14 0
      , started = False
      , width = 1280
      , height = 768
      , device = classifyDevice { width = 1280, height = 768 }
      }
    , Cmd.batch
        [ Http.get
            { url = "poemas.txt"
            , expect = Http.expectString GotText
            }
        , Task.perform (\{ viewport } -> InitialScreenSize (round viewport.width) (round viewport.height)) getViewport
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Events.onKeyDown keyDecoder
        , Events.onClick (Decode.succeed <| KeyboardMsg Enter)
        , Events.onResize <|
            \width height ->
                Resized ( width, height, classifyDevice { width = width, height = height } )
        ]


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
    | Resized ( Int, Int, Device )
    | InitialScreenSize Int Int


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

        Resized ( w, h, d ) ->
            ( { model | width = w, height = h, device = d }, Cmd.none )

        InitialScreenSize w h ->
            ( { model | width = w, height = h, device = classifyDevice { width = w, height = h } }, Cmd.none )


linePadding line =
    paddingEach
        { top = 0
        , right = 0
        , bottom =
            if List.member line [ 3, 7, 10 ] then
                35

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


renderIntro model =
    column
        [ width fill
        , centerY
        , Font.center
        , responsiveFontSize model.width 4
        ]
        [ el [ width fill, padding 50, responsiveFontSize model.width 14 ] <| text "Algo Rimo"
        , el [ width fill ] <| text "generador algorítimo de"
        , el [ width fill, padding 20 ] <| text "sonetos alejandrinos"
        , textColumn [ width fill, spacing 10, padding 10 ]
            [ paragraph []
                [ el [ responsiveFontSize model.width 10, padding 10 ] (text "⏎")
                , el [ padding 10 ] <| text "genera un nuevo soneto"
                ]
            , paragraph []
                [ el [ responsiveFontSize model.width 6, padding 20 ] (text "⇦⇧⇨⇩")
                , text "cambia los versos"
                ]
            ]
        ]


renderPoems model fullText =
    column
        [ width fill
        , centerY
        , spacing 14
        , Font.center
        ]
    <|
        List.map
            (mapLines fullText model.index model.indexes)
            (List.range 0 13)


responsiveFontSize : Int -> Int -> Attr decorative msg
responsiveFontSize w m =
    let
        width =
            if w > 800 then
                800

            else if w < 400 then
                400

            else
                w
    in
    Font.size <| (width * m) // 100



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.image "bg.jpg"
        , width fill
        , height fill
        , responsiveFontSize model.width 4
        , Font.family
            [ Font.external
                { url = "https://fonts.googleapis.com/css?family=Beth+Ellen&display=swap"
                , name = "Beth Ellen"
                }
            , Font.sansSerif
            ]
        ]
    <|
        case model.status of
            Failure ->
                el [ width fill, height fill ] <| text "Error. No se encuentran los poemas."

            Loading ->
                el [ width fill, height fill ] <| text "Cargando poemas..."

            Success fullText ->
                if model.started then
                    renderPoems model fullText

                else
                    renderIntro model
