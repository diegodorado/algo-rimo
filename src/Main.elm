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



-- port wrapper


playClip : String -> Model -> Cmd msg
playClip clip model =
    if model.started && model.soundOn then
        playSound clip

    else
        Cmd.none


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
    , soundOn : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { index = 0
      , status = Loading
      , indexes = List.repeat 14 0
      , started = False
      , soundOn = True
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

        "a" ->
            ArrowLeft

        "d" ->
            ArrowRight

        "w" ->
            ArrowUp

        "s" ->
            ArrowDown

        "Enter" ->
            Enter

        _ ->
            OtherKey


changeIndexes indexes line next =
    let
        change l idx =
            if l == line then
                modBy 10
                    (if next then
                        idx + 1

                     else
                        idx - 1
                    )

            else
                idx
    in
    List.indexedMap change indexes


nextPoemLine model line =
    let
        idxs =
            changeIndexes model.indexes line True
    in
    ( { model | indexes = idxs }, playClip "change" model )


prevPoemLine model line =
    let
        idxs =
            changeIndexes model.indexes line False
    in
    ( { model | indexes = idxs }, playClip "change" model )



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
    | ChangeLine Int
    | GotText (Result Http.Error String)
    | KeyboardMsg KeyboardMsg
    | ToggleSound
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

        ChangeLine line ->
            let
                idxs =
                    changeIndexes model.indexes line False
            in
            ( { model | indexes = idxs, index = line }, playClip "change" model )

        RandomIndexes idxs ->
            ( { model | indexes = idxs }, Cmd.none )

        ToggleSound ->
            ( { model | soundOn = not model.soundOn }, Cmd.none )

        KeyboardMsg key ->
            case key of
                ArrowLeft ->
                    prevPoemLine model model.index

                ArrowRight ->
                    nextPoemLine model model.index

                ArrowUp ->
                    ( { model | index = modBy 14 (model.index - 1) }, playClip "select" model )

                ArrowDown ->
                    ( { model | index = modBy 14 (model.index + 1) }, playClip "select" model )

                Enter ->
                    ( { model | started = True }
                    , Cmd.batch
                        [ Random.generate RandomIndexes (Random.list 14 (Random.int 0 9))
                        , playClip "random" model
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
        [ Input.button
            [ fc
            , width fill
            , responsiveLinePadding model.width model.height 1.3 line
            ]
            { onPress = Just <| ChangeLine line
            , label = text (getLine line idx fulltext)
            }
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
        , spacing 20
        ]
        [ el [ centerX, padding 20, responsiveFontSize model.width model.height 6 ] <| text "Algo Rimo"
        , el
            [ centerX
            , padding 10
            , responsiveFontSize model.width model.height 1.4
            ]
          <|
            text "Sonetos generados algorítmicamente"
        , column
            [ centerX
            , responsiveFontSize model.width model.height 1
            , Font.family
                [ Font.typeface "Courier"
                , Font.sansSerif
                ]
            , padding 15
            , spacing 15
            , Border.dashed
            , Border.width 2
            ]
            [ row [ width fill, centerX, spacing 15 ]
                [ image [ width <| px 90 ]
                    { src = "assets/svg/enter-key.svg"
                    , description = "enter"
                    }
                , el [ alignBottom ] <| text "genera un nuevo soneto"
                ]
            , row [ width fill, centerX, spacing 15 ]
                [ image [ width <| px 100 ]
                    { src = "assets/svg/wasd-keys.svg"
                    , description = "enter"
                    }
                , el [ alignBottom, padding 10 ] <| text "cambia los versos"
                ]
            ]
        , Input.button [ Border.rounded 20, Background.color <| rgb 0 0 0, Font.color (rgb 1 1 1), centerX, padding 20, responsiveFontSize model.width model.height 2.5 ]
            { onPress = Just <| KeyboardMsg Enter
            , label = text "Jugar"
            }
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
            (mapLines model fullText)
            (List.range 0 13)


renderSoundCtrl on =
    Input.button [ padding 20, alignTop ]
        { onPress = Just ToggleSound
        , label =
            image
                [ width <| px 30 ]
                { src =
                    String.concat
                        [ "assets/svg/sound-"
                        , if on then
                            "on"

                          else
                            "off"
                        , ".svg"
                        ]
                , description = "enter"
                }
        }


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
                row
                    [ width fill
                    , height fill
                    ]
                    [ el [ padding 20 ] <| el [ width <| px 30, height <| px 30 ] none
                    , if model.started then
                        renderPoems model fullText

                      else
                        renderIntro model
                    , renderSoundCtrl model.soundOn
                    ]
