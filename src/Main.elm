module Main exposing (main)

import Browser
import Css as C
import Html.Styled as H
import Html.Styled.Attributes as HA
import Html.Styled.Events as HE
import Json.Decode


main : Program () Model Msg
main =
    Browser.sandbox { init = 21, update = update, view = view >> H.toUnstyled }


type alias Model =
    Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        WidthSlid val ->
            case String.toInt val of
                Just n ->
                    n

                Nothing ->
                    model


type Msg
    = WidthSlid String


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.label [ HA.for "featureWidth" ]
            [ H.text "feature width"
            , H.input
                [ HA.type_ "range"
                , HA.min "9"
                , HA.max "105"
                , HA.step "4"
                , HA.value <| String.fromInt model
                , onInput WidthSlid
                ]
                []
            ]
            |> List.singleton
            |> H.div []
        , H.div [] [ H.text <| String.fromInt model ]
        , rows model
            |> H.tbody []
            |> List.singleton
            |> H.table
                []
        ]


rows : Int -> List (H.Html msg)
rows width =
    width
        // 2
        |> List.range 0
        |> List.map
            (stitches width)
        |> mirror
        |> List.indexedMap (addRowNumberToRow width)


addRowNumberToRow : Int -> Int -> List (H.Html msg) -> H.Html msg
addRowNumberToRow width idx r =
    H.tr [] <| rowTitle (width - idx) :: r


row : Int -> Int -> H.Html msg
row width idx =
    H.tr [] <| stitches width idx


rowTitle : Int -> H.Html msg
rowTitle n =
    H.th [] [ H.text <| String.fromInt n ]


stitches : Int -> Int -> List (H.Html msg)
stitches width rowIdx =
    width
        // 2
        |> List.range 0
        |> List.map
            (stitch (width // 2) rowIdx)
        |> mirror


stitch : Int -> Int -> Int -> H.Html msg
stitch width rowIdx n =
    let
        _ =
            Debug.log "stitching" ( width, rowIdx, n )
    in
    (if n > (width - rowIdx - 1) then
        stockingStitch rowIdx

     else
        seedStitch rowIdx n
    )
        |> stitchToCell


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
                        C.hsl 100 0.5 0.5

                    WrongKnit ->
                        C.hsl 200 0.7 0.7

                    RightPurl ->
                        C.hsl 200 0.5 0.5

                    WrongPurl ->
                        C.hsl 100 0.7 0.7
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
