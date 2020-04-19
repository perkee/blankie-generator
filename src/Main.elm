module Main exposing (main)

import Browser
import Css as C
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view >> H.toUnstyled }


type alias Model =
    { featureWidth : Int
    , horizontalPadding : Int
    , verticalPadding : Int
    }


init : Model
init =
    { featureWidth = 21
    , horizontalPadding = 10
    , verticalPadding = 10
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        FeatureWidthSlid val ->
            case String.toInt val of
                Just n ->
                    { model | featureWidth = n }

                Nothing ->
                    model

        HorizontalPaddingSlid val ->
            case String.toInt val of
                Just n ->
                    { model | horizontalPadding = n }

                Nothing ->
                    model


type Msg
    = FeatureWidthSlid String
    | HorizontalPaddingSlid String


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.label [ HA.for "featureWidth" ]
            [ H.div []
                [ H.text "feature width"
                , H.input
                    [ HA.type_ "range"
                    , HA.id "featureWidth"
                    , HA.min "9"
                    , HA.max "105"
                    , HA.step "4"
                    , HA.value <| String.fromInt model.featureWidth
                    , onInput FeatureWidthSlid
                    ]
                    []
                , H.span [] [ H.text <| String.fromInt model.featureWidth ]
                ]
            ]
        , H.label [ HA.for "horizontalPadding" ]
            [ H.div []
                [ H.text "horizontalPadding"
                , H.input
                    [ HA.type_ "range"
                    , HA.id "horizontalPadding"
                    , HA.min "0"
                    , HA.max "20"
                    , HA.step "2"
                    , HA.value <| String.fromInt model.horizontalPadding
                    , onInput HorizontalPaddingSlid
                    ]
                    []
                , H.span [] [ H.text <| String.fromInt model.horizontalPadding ]
                ]
            ]
            |> List.singleton
            |> H.div []
        , rows model
            |> H.tbody []
            |> List.singleton
            |> H.table
                []
        ]


rows : Model -> List (H.Html msg)
rows model =
    model.featureWidth
        // 2
        |> List.range 0
        |> List.map
            (stitches model)
        |> mirror
        |> List.indexedMap (addRowNumberToRow model.featureWidth)


addRowNumberToRow : Int -> Int -> List (H.Html msg) -> H.Html msg
addRowNumberToRow width idx r =
    H.tr [] <| rowTitle (width - idx) :: r


rowTitle : Int -> H.Html msg
rowTitle n =
    H.th [] [ H.text <| String.fromInt n ]


stitches : Model -> Int -> List (H.Html msg)
stitches { featureWidth, horizontalPadding } rowIdx =
    featureWidth
        // 2
        |> List.range 0
        |> List.map
            (stitch (featureWidth // 2) rowIdx)
        |> (++) (List.range 0 (horizontalPadding - 1) |> List.map (seedStitch rowIdx))
        |> mirror
        |> List.map stitchToCell


stitch : Int -> Int -> Int -> Stitch
stitch width rowIdx n =
    let
        _ =
            Debug.log "stitching" ( width, rowIdx, n )
    in
    if n > (width - rowIdx - 1) then
        stockingStitch rowIdx

    else
        seedStitch rowIdx n


stitchStringToCell : String -> H.Html msg
stitchStringToCell s =
    H.td
        [ HA.class <| "stitch--" ++ s
        ]
        [ H.text s
        ]


mirror : List a -> List a
mirror list =
    list ++ (List.reverse list |> List.drop 1)


onInput : (String -> msg) -> H.Attribute msg
onInput handler =
    HE.on "input" <| Json.Decode.map handler <| Json.Decode.at [ "target", "value" ] Json.Decode.string


seedStitch : Int -> Int -> Stitch
seedStitch row_ col =
    case ( parity row_, parity col ) of
        ( Even, Even ) ->
            RightKnit

        ( Odd, Even ) ->
            WrongKnit

        ( Even, Odd ) ->
            RightPurl

        ( Odd, Odd ) ->
            WrongPurl


stockingStitch : Int -> Stitch
stockingStitch row_ =
    case parity row_ of
        Even ->
            RightKnit

        Odd ->
            WrongPurl


parity : Int -> Parity
parity n =
    if modBy 2 n == 1 then
        Odd

    else
        Even


type Parity
    = Odd
    | Even


type Stitch
    = RightKnit
    | WrongKnit
    | RightPurl
    | WrongPurl


stitchToCell : Stitch -> H.Html msg
stitchToCell s =
    H.td
        [ HA.css
            [ C.backgroundColor <|
                case s of
                    RightKnit ->
                        C.hsl 90 0.5 0.5

                    WrongKnit ->
                        C.hsl 270 0.7 0.7

                    RightPurl ->
                        C.hsl 270 0.5 0.5

                    WrongPurl ->
                        C.hsl 90 0.7 0.7
            ]
        ]
        [ stitchToString s |> H.text
        ]


stitchToString : Stitch -> String
stitchToString s =
    case s of
        RightKnit ->
            "K"

        WrongKnit ->
            "N"

        RightPurl ->
            "P"

        WrongPurl ->
            "R"
