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
import Html.Attributes
import Http
import Json.Decode as Decode
import Random
import Task


port playSound : String -> Cmd msg



-- port wrapper


playClip : String -> Model -> Cmd msg
playClip clip model =
    if model.soundOn then
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


type Page
    = Home
    | Info
    | Game


type Theme
    = HandWriting
    | Computer
    | Book


type ThemeColor
    = Primary
    | Secondary
    | Contrast


type alias Model =
    { index : Int
    , indexes : List Int
    , status : Status
    , page : Page
    , theme : Theme
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
      , page = Home
      , theme = HandWriting
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

        " " ->
            SpaceBar

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
    | SpaceBar
    | OtherKey


type Msg
    = Increment Int
    | Decrement Int
    | ChangeLine Int
    | GotText (Result Http.Error String)
    | KeyboardMsg KeyboardMsg
    | ToggleSound
    | Restart
    | ShowInfo
    | ChangeTheme
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

        ShowInfo ->
            ( { model | page = Info }, Cmd.none )

        ChangeTheme ->
            case model.theme of
                HandWriting ->
                    ( { model | theme = Computer }, Cmd.none )

                Computer ->
                    ( { model | theme = Book }, Cmd.none )

                Book ->
                    ( { model | theme = HandWriting }, Cmd.none )

        Restart ->
            ( { model | page = Home }, Cmd.none )

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

                SpaceBar ->
                    ( { model | page = Game }
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
                Font.color <| themedFg model.theme Secondary

            else
                Font.color <| themedFg model.theme Primary
    in
    row
        [ width fill ]
        [ Input.button
            [ fc
            , width fill
            , responsiveLinePadding model.width model.height 1.75 line
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


keyBox : Model -> String -> Maybe Int -> Element msg
keyBox model t s =
    let
        w =
            case s of
                Nothing ->
                    40

                Just ww ->
                    ww
    in
    el
        [ width <| px w
        , height <| px 40
        , Border.rounded 5
        , Font.bold
        , Background.color <| themedFg model.theme Primary
        , Font.color <| themedFg model.theme Contrast
        , Font.size 22
        , padding 10
        , centerX
        ]
    <|
        text t


wasdKeys : Model -> Element msg
wasdKeys model =
    column
        [ spacing 5
        ]
        [ keyBox model "W" Nothing
        , row [ spacing 5 ]
            [ keyBox model "A" Nothing
            , keyBox model "S" Nothing
            , keyBox model "D" Nothing
            ]
        ]


spaceKey : Model -> Element msg
spaceKey model =
    el
        [ spacing 5
        ]
    <|
        keyBox model "SPACE" <|
            Just 130


renderIntro model =
    let
        roboto =
            Font.family [ Font.typeface "Roboto", Font.sansSerif ]
    in
    column
        [ width fill
        , centerY
        , Font.center
        , Font.color <| themedFg model.theme Primary
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
            , responsiveFontSize model.width model.height 1.4
            , padding 15
            , spacing 15
            , roboto
            , Border.dashed
            , Border.width 2
            ]
            [ row [ width fill, centerX, spacing 15 ]
                [ spaceKey model
                , el [ alignBottom ] <| text "genera un nuevo soneto"
                ]
            , row [ width fill, centerX, spacing 15 ]
                [ wasdKeys model
                , el [ alignBottom, padding 10 ] <| text "cambia los versos"
                ]
            ]
        , Input.button [ Border.rounded 20, Background.color <| themedFg model.theme Primary, Font.color <| themedFg model.theme Contrast, centerX, padding 20, responsiveFontSize model.width model.height 2.5 ]
            { onPress = Just <| KeyboardMsg SpaceBar
            , label = text "Jugar"
            }
        ]


renderInfo model =
    column
        [ width fill
        , centerY
        , Font.color <| themedFg model.theme Primary
        , Font.justify
        , padding 20
        ]
        [ el [ centerX, padding 20, responsiveFontSize model.width model.height 3 ] <| text "Algo Rimo"
        , column
            [ centerX
            , paddingXY 60 20
            , responsiveFontSize model.width model.height 1.2
            , Font.family
                [ Font.typeface "Roboto"
                , Font.sansSerif
                ]
            , spacing 20
            ]
            [ paragraph
                []
                [ text "Desarrollada por "
                , el [ Font.bold ] (text "educ.ar")
                , text ", Algo Rimo es una aplicación web para PC y dispositivos móviles que genera sonetos de forma aleatoria a partir de un corpus compuesto por estrofas de sonetos en español que se encuentran en el dominio público. Los sonetos generados siguen el esquema "
                , el [ Font.bold ] (text "ABBA ABBA CDC CDC")
                , text "."
                ]
            , paragraph
                []
                [ el [ alignLeft, Font.center ] <| text "Quien lee tiene dos caminos:" ]
            , paragraph
                []
                [ el [] <| text " • leer un "
                , el [ Font.bold ] (text "soneto generado algorítmicamente")
                , el [] <| text " (con versos seleccionados por la aplicación) y disfrutar de una composición producto del azar, como quien arroja un dado y espera ver qué le ha tocado en suerte;"
                ]
            , paragraph
                []
                [ el [] <| text " • generar un "
                , el [ Font.bold ] (text "soneto personalizado")
                , el [] <| text ", seleccionando cada uno de sus catorce versos (el algoritmo propone y quien lee dispone). En PC, la selección se realiza con las teclas "
                , el [ Font.bold ] <| text "W"
                , el [] <| text " (subir), "
                , el [ Font.bold ] <| text "S"
                , el [] <| text " (bajar), "
                , el [ Font.bold ] <| text "A"
                , el [] <| text "  (izquierda) y "
                , el [ Font.bold ] <| text "D"
                , el [] <| text " (derecha) y, desde un dispositivo móvil, tocando el verso que se quiere cambiar."
                ]
            , paragraph
                []
                [ text "Algo Rimo está inspirada en el famoso libro "
                , el [ Font.italic ] <| text "Cien mil millones de poemas"
                , text ", publicado en 1961 por "
                , link [ Font.bold ]
                    { url = "https://es.wikipedia.org/wiki/Raymond_Queneau"
                    , label = text "Raymond Queneau"
                    }
                , text ", un escritor francés formado en el surrealismo. El libro consta solamente de diez páginas, en cada una de las cuales está impreso un soneto. La particularidad es que cada verso está cortado como si fuera una página en sí mismo, de modo que los versos de los diez sonetos se pueden combinar de tantas maneras que llevaría siglos terminar de leerlos todos. Un experimento literario y un juego a la vez."
                ]
            , paragraph
                []
                [ text "En tiempos de novelas escritas por algoritmos, sonetos que imitan el estilo de Shakespeare y noticias generadas automáticamente por bots, es lícito preguntarse una vez más por la relación entre máquinas y humanos y por la diferencia entre imitación y arte."
                ]
            , paragraph
                []
                [ text "¿Es posible enseñarle a una máquina a escribir poesía? ¿Hay revelación en un soneto generado al azar? ¿Puede un algoritmo ser tan creativo como una persona de carne y hueso? Estas son algunas de las preguntas que, en la actualidad, se están haciendo investigadores del campo de la inteligencia artificial y de las letras. Preguntas que Algo Rimo busca avivar."
                ]
            ]
        , Input.button [ Border.rounded 20, Background.color <| themedFg model.theme Primary, Font.color <| themedFg model.theme Contrast, centerX, padding 20, responsiveFontSize model.width model.height 2 ]
            { onPress = Just <| KeyboardMsg SpaceBar
            , label = text "Jugar"
            }
        ]


responsiveSpacing : Int -> Int -> Float -> Attribute msg
responsiveSpacing w h m =
    spacing <| round <| (m * toFloat h / toFloat w)


renderPoems model fullText =
    column
        [ width fill
        , centerY
        , responsiveSpacing model.width model.height 20
        , Font.center
        ]
    <|
        List.map
            (mapLines model fullText)
            (List.range 0 13)


renderMenu model =
    column [ alignRight, alignTop, spacing 20, padding 20 ]
        [ Input.button []
            { onPress = Just ToggleSound
            , label =
                el [ Font.size 28, Font.color <| themedFg model.theme Primary ] <|
                    faIcon <|
                        if model.soundOn then
                            "volume-up"

                        else
                            "volume-mute"
            }
        , if model.page /= Home then
            Input.button []
                { onPress = Just Restart
                , label =
                    el [ Font.size 28, Font.color <| themedFg model.theme Primary ] <| faIcon "home"
                }

          else
            none
        , Input.button []
            { onPress = Just ShowInfo
            , label =
                el [ Font.size 28, Font.color <| themedFg model.theme Primary ] <| faIcon "info-circle"
            }
        , Input.button []
            { onPress = Just ChangeTheme
            , label =
                el [ Font.size 28, Font.color <| themedFg model.theme Primary ] <| faIcon "eye"
            }
        ]


faIcon icon =
    html (Html.i [ Html.Attributes.class <| String.concat [ "fas fa-", icon ] ] [])


responsiveFontSize : Int -> Int -> Float -> Attr decorative msg
responsiveFontSize w h m =
    Font.size <| round <| (m * toFloat (min maxWidth w) + m * toFloat h) / 100



-- VIEW


maxWidth =
    768


maxHeight =
    768


themedFont theme =
    case theme of
        HandWriting ->
            Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Beth+Ellen&display=swap"
                    , name = "Beth Ellen"
                    }
                , Font.sansSerif
                ]

        Computer ->
            Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=VT323&display=swap"
                    , name = "VT323"
                    }
                , Font.sansSerif
                ]

        Book ->
            Font.family
                [ Font.external
                    { url = "https://fonts.googleapis.com/css?family=Cormorant&display=swap"
                    , name = "Cormorant"
                    }
                , Font.serif
                ]


themedBg theme =
    case theme of
        HandWriting ->
            Background.image "assets/img/bg.jpg"

        Computer ->
            Background.color <| rgb 0 0 0

        Book ->
            Background.color <| rgb 1 1 1


themedFg theme col =
    case theme of
        HandWriting ->
            case col of
                Primary ->
                    rgb 0 0 0

                Secondary ->
                    rgb 0 1 0

                Contrast ->
                    rgb 1 1 1

        Computer ->
            case col of
                Primary ->
                    rgb 0 1 0

                Secondary ->
                    rgb 0 1 1

                Contrast ->
                    rgb 0 0 0

        Book ->
            case col of
                Primary ->
                    rgb 0 0 0

                Secondary ->
                    rgb 0 1 0

                Contrast ->
                    rgb 1 1 1


view : Model -> Html Msg
view model =
    Element.layout
        [ themedBg model.theme
        , width fill
        , responsiveFontSize model.width model.height 1.2
        , themedFont model.theme
        , inFront <| renderMenu model
        ]
    <|
        el
            [ centerX
            , centerY
            , width (fill |> maximum maxWidth)

            -- , height (fill |> maximum maxHeight)
            , paddingXY 0 30
            ]
        <|
            case model.status of
                Failure ->
                    el [ width fill, height fill ] <| text "Error. No se encuentran los poemas."

                Loading ->
                    el [ width fill, height fill ] <| text "Cargando poemas..."

                Success fullText ->
                    case model.page of
                        Home ->
                            renderIntro model

                        Info ->
                            renderInfo model

                        Game ->
                            renderPoems model fullText
