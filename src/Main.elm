module Main exposing (Msg(..), main, update, view)

import Browser
import Duration exposing (Seconds)
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes as Attr exposing (for, id, type_, value)
import Html.Events exposing (onInput)
import Length exposing (Length, Meters)
import Quantity exposing (Quantity, Rate)
import Round


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { distanceInKilometers : String
    , distanceInMiles : String
    , pacePerKmMinutes : String
    , pacePerKmSeconds : String
    , pacePerMileMinutes : String
    , pacePerMileSeconds : String
    }


type Msg
    = SetKilometers String
    | SetMiles String
    | SetPacePerKmMinutes String
    | SetPacePerKmSeconds String
    | SetPacePerMileMinutes String
    | SetPacePerMileSeconds String


init : Model
init =
    let
        distance =
            Length.kilometers 10

        pace =
            minutesPerKilometer 6
    in
    emptyModel
        |> updateDistance distance
        |> updatePace pace


emptyModel : Model
emptyModel =
    { distanceInKilometers = ""
    , distanceInMiles = ""
    , pacePerKmMinutes = ""
    , pacePerKmSeconds = ""
    , pacePerMileMinutes = ""
    , pacePerMileSeconds = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetKilometers s ->
            let
                newModel =
                    s
                        |> String.toFloat
                        |> Maybe.map Length.kilometers
                        |> Maybe.map (\distance -> updateDistance distance model)
                        |> Maybe.withDefault model
            in
            { newModel | distanceInKilometers = s }

        SetMiles s ->
            let
                newModel =
                    s
                        |> String.toFloat
                        |> Maybe.map Length.miles
                        |> Maybe.map (\distance -> updateDistance distance model)
                        |> Maybe.withDefault model
            in
            { newModel | distanceInMiles = s }

        SetPacePerKmMinutes s ->
            let
                seconds =
                    model.pacePerKmSeconds |> String.toInt |> Maybe.withDefault 0

                newModel =
                    s
                        |> String.toInt
                        |> Maybe.map (\minutes -> minutesAndSecondsPerKilometer minutes seconds)
                        |> Maybe.map (\pace -> updatePace pace model)
                        |> Maybe.withDefault model
            in
            { newModel | pacePerKmMinutes = s }

        SetPacePerKmSeconds s ->
            let
                minutes =
                    model.pacePerKmMinutes |> String.toInt |> Maybe.withDefault 0

                newModel =
                    s
                        |> String.toInt
                        |> Maybe.map (\seconds -> minutesAndSecondsPerKilometer minutes seconds)
                        |> Maybe.map (\pace -> updatePace pace model)
                        |> Maybe.withDefault model
            in
            { newModel | pacePerKmSeconds = s }

        SetPacePerMileMinutes s ->
            let
                seconds =
                    model.pacePerMileSeconds |> String.toInt |> Maybe.withDefault 0

                newModel =
                    s
                        |> String.toInt
                        |> Maybe.map (\minutes -> minutesAndSecondsPerMile minutes seconds)
                        |> Maybe.map (\pace -> updatePace pace model)
                        |> Maybe.withDefault model
            in
            { newModel | pacePerMileMinutes = s }

        SetPacePerMileSeconds s ->
            let
                minutes =
                    model.pacePerMileMinutes |> String.toInt |> Maybe.withDefault 0

                newModel =
                    s
                        |> String.toInt
                        |> Maybe.map (\seconds -> minutesAndSecondsPerMile minutes seconds)
                        |> Maybe.map (\pace -> updatePace pace model)
                        |> Maybe.withDefault model
            in
            { newModel | pacePerMileSeconds = s }


updateDistance : Length -> Model -> Model
updateDistance distance model =
    { model
        | distanceInKilometers = roundForDisplay <| Length.inKilometers distance
        , distanceInMiles = roundForDisplay <| Length.inMiles distance
    }


updatePace : Pace -> Model -> Model
updatePace pace form =
    let
        ( pacePerKmMinutes, pacePerKmSeconds ) =
            inMinutesAndSecondsPerKilometer pace

        ( pacePerMileMinutes, pacePerMileSeconds ) =
            inMinutesAndSecondsPerMile pace
    in
    { form
        | pacePerKmMinutes = String.fromInt pacePerKmMinutes
        , pacePerKmSeconds = String.fromInt pacePerKmSeconds
        , pacePerMileMinutes = String.fromInt pacePerMileMinutes
        , pacePerMileSeconds = String.fromInt pacePerMileSeconds
    }


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [ for "rpc-field-km" ] [ text "Distance in km" ]
            , input
                [ type_ "number"
                , value model.distanceInKilometers
                , onInput SetKilometers
                , id "rpc-field-km"
                , Attr.min "0"
                ]
                []
            ]
        , div []
            [ label [ for "rpc-field-miles" ] [ text "Distance in miles" ]
            , input
                [ type_ "number"
                , value model.distanceInMiles
                , onInput SetMiles
                , id "rpc-field-miles"
                , Attr.min "0"
                ]
                []
            ]
        , div []
            [ label [] [ text "Pace in min/km" ]
            , input
                [ type_ "number"
                , Attr.min "0"
                , value model.pacePerKmMinutes
                , onInput SetPacePerKmMinutes
                ]
                []
            , span [] [ text "m" ]
            , input
                [ type_ "number"
                , Attr.min "0"
                , Attr.max "59"
                , value model.pacePerKmSeconds
                , onInput SetPacePerKmSeconds
                ]
                []
            , span [] [ text "s" ]
            ]
        , div []
            [ label [] [ text "Pace in min/mi" ]
            , input
                [ type_ "number"
                , Attr.min "0"
                , value model.pacePerMileMinutes
                , onInput SetPacePerMileMinutes
                ]
                []
            , span [] [ text "m" ]
            , input
                [ type_ "number"
                , Attr.min "0"
                , Attr.max "59"
                , value model.pacePerMileSeconds
                , onInput SetPacePerMileSeconds
                ]
                []
            , span [] [ text "s" ]
            ]
        , div []
            [ label [] [ text "Speed in km/h" ]
            , input [ type_ "number" ] []
            ]
        , div []
            [ label [] [ text "Speed in mi/h" ]
            , input [ type_ "number" ] []
            ]
        , div []
            [ label [] [ text "Total Time" ]
            , input [ type_ "number" ] []
            , span [] [ text "h" ]
            , input [ type_ "number" ] []
            , span [] [ text "m" ]
            , input [ type_ "number" ] []
            , span [] [ text "s" ]
            ]
        ]


roundForDisplay : Float -> String
roundForDisplay n =
    n
        |> Round.round 3
        |> String.trim



-- Pace


type alias SecondsPerMeter =
    Rate Seconds Meters


type alias Pace =
    Quantity Float SecondsPerMeter


secondsPerMeter : Float -> Pace
secondsPerMeter numSecondsPerMeter =
    Quantity.Quantity numSecondsPerMeter


minutesPerKilometer : Float -> Pace
minutesPerKilometer numMinutesPerKilometer =
    secondsPerMeter ((numMinutesPerKilometer * 60) / 1000)


inSecondsPerKilometer : Pace -> Float
inSecondsPerKilometer (Quantity.Quantity numSecondsPerMeter) =
    numSecondsPerMeter * 1000


minutesAndSecondsPerKilometer : Int -> Int -> Pace
minutesAndSecondsPerKilometer minutes seconds =
    secondsPerMeter
        ((toFloat minutes * 60 + toFloat seconds) / 1000)


inMinutesAndSecondsPerKilometer : Pace -> ( Int, Int )
inMinutesAndSecondsPerKilometer pace =
    pace |> inSecondsPerKilometer |> toMinutesAndSeconds


inSecondsPerMile : Pace -> Float
inSecondsPerMile (Quantity.Quantity numSecondsPerMeter) =
    numSecondsPerMeter * mileInMeters


minutesAndSecondsPerMile : Int -> Int -> Pace
minutesAndSecondsPerMile minutes seconds =
    secondsPerMeter
        ((toFloat minutes * 60 + toFloat seconds) / mileInMeters)


inMinutesAndSecondsPerMile : Pace -> ( Int, Int )
inMinutesAndSecondsPerMile pace =
    pace |> inSecondsPerMile |> toMinutesAndSeconds


toMinutesAndSeconds : Float -> ( Int, Int )
toMinutesAndSeconds totalSeconds =
    let
        totalMinutes =
            totalSeconds / 60

        minutes =
            floor totalMinutes

        remainingMinutes =
            (totalSeconds / 60) - toFloat minutes

        seconds =
            round (remainingMinutes * 60)
    in
    ( minutes, seconds )


mileInMeters : Float
mileInMeters =
    5280 * 12 * 0.0254
