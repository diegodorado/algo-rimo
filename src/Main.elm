port module Main exposing (main, playSound)

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


port playSound : String -> Cmd msg


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
            { url = "assets/poemas.txt"
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
    ( { model | indexes = idxs }, playSound "change" )


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
    ( { model | indexes = idxs }, playSound "change" )



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
                    ( { model | index = modBy 14 (model.index - 1) }, playSound "select" )

                ArrowDown ->
                    ( { model | index = modBy 14 (model.index + 1) }, playSound "select" )

                Enter ->
                    ( { model | started = True }
                    , Cmd.batch
                        [ Random.generate RandomIndexes (Random.list 14 (Random.int 0 9))
                        , playSound "random"
                        ]
                    )

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


responsiveLinePadding : Int -> Int -> Float -> Int -> Attribute msg
responsiveLinePadding w h m line =
    paddingEach
        { top = 0
        , right = 0
        , bottom =
            if List.member line [ 3, 7, 10 ] then
                round <| (m * toFloat w + m * toFloat h) / 100

            else
                0
        , left = 0
        }


renderLine line idx model fulltext =
    let
        fc =
            if line == model.index then
                Font.color (rgb 0.0 0.7 0.7)

            else
                Font.color (rgb 0 0 0)
    in
    row [ width fill ]
        [ el
            [ fc
            , width fill
            , responsiveLinePadding model.width model.height 1.5 line
            ]
            (text (getLine line idx fulltext))
        ]


mapLines model fullText line =
    let
        idx =
            Maybe.withDefault 0 <| Array.get line <| Array.fromList <| model.indexes
    in
    renderLine line idx model fullText


renderIntro model =
    column
        [ width fill
        , centerY
        , Font.center
        , responsiveFontSize model.width model.height 1.2
        ]
        [ el [ centerX, padding 50, responsiveFontSize model.width model.height 6 ] <| text "Algo Rimo"
        , el [ centerX ] <| text "generador algorítmico de"
        , el [ centerX, padding 20 ] <| text "sonetos alejandrinos"
        , el [ padding 20 ] none
        , column
            [ centerX
            , responsiveFontSize model.width model.height 0.8
            , Font.family
                [ Font.typeface "Courier"
                , Font.sansSerif
                ]
            , padding 20
            , spacing 15
            , Border.dashed
            , Border.width 2
            ]
            [ row [ centerX, spacing 15 ]
                [ image [ width <| px 55 ]
                    { src = "assets/svg/enter-key.svg"
                    , description = "enter"
                    }
                , el [ alignBottom ] <| text "genera un nuevo soneto"
                ]
            , row [ centerX, spacing 15 ]
                [ image [ width <| px 100 ]
                    { src = "assets/svg/arrow-keys.svg"
                    , description = "enter"
                    }
                , el [ alignBottom, padding 10 ] <| text "cambia los versos"
                ]
            ]
        ]


renderPoems model fullText =
    column
        [ width fill
        , centerY
        , spacing 10
        , Font.center
        ]
    <|
        List.map
            (mapLines model fullText)
            (List.range 0 13)


responsiveFontSize : Int -> Int -> Float -> Attr decorative msg
responsiveFontSize w h m =
    Font.size <| round <| (m * toFloat w + m * toFloat h) / 100



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.image "assets/img/bg.jpg"
        , width fill
        , height fill
        , responsiveFontSize model.width model.height 1.2
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
