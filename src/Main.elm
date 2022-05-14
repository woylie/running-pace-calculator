module Main exposing (Msg(..), main, update, view)

import Browser
import Duration exposing (Duration, Seconds, inSeconds, seconds)
import Html exposing (Html, div, input, label, span, text)
import Html.Attributes as Attr exposing (for, id, type_, value)
import Html.Events exposing (onInput)
import Length exposing (Length, Meters, inMeters)
import Quantity exposing (Quantity, Rate)
import Round
import Speed
    exposing
        ( Speed
        , inKilometersPerHour
        , inMilesPerHour
        , kilometersPerHour
        , metersPerSecond
        , milesPerHour
        )


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
    , speedInKmh : String
    , speedInMph : String
    , durationHours : String
    , durationMinutes : String
    , durationSeconds : String
    }


type Msg
    = SetKilometers String
    | SetMiles String
    | SetPacePerKmMinutes String
    | SetPacePerKmSeconds String
    | SetPacePerMileMinutes String
    | SetPacePerMileSeconds String
    | SetSpeedInKmh String
    | SetSpeedInMph String
    | SetTotalTimeHours String
    | SetTotalTimeMinutes String
    | SetTotalTimeSeconds String


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
    , speedInKmh = ""
    , speedInMph = ""
    , durationHours = ""
    , durationMinutes = ""
    , durationSeconds = ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetKilometers s ->
            case String.toFloat s of
                Just km ->
                    updateDistance (Length.kilometers km) model

                Nothing ->
                    { model | distanceInKilometers = s }

        SetMiles s ->
            case String.toFloat s of
                Just mi ->
                    updateDistance (Length.miles mi) model

                Nothing ->
                    { model | distanceInMiles = s }

        SetPacePerKmMinutes s ->
            case String.toInt s of
                Just minutes ->
                    let
                        seconds =
                            parseIntWithDefault model.pacePerKmSeconds
                    in
                    updatePace
                        (minutesAndSecondsPerKilometer minutes seconds)
                        model

                Nothing ->
                    { model | pacePerKmMinutes = s }

        SetPacePerKmSeconds s ->
            case String.toInt s of
                Just seconds ->
                    let
                        minutes =
                            parseIntWithDefault model.pacePerKmMinutes

                        pace =
                            if minutes == 0 then
                                minutesAndSecondsPerKilometer
                                    minutes
                                    (max seconds 0)

                            else
                                minutesAndSecondsPerKilometer
                                    minutes
                                    seconds
                    in
                    updatePace pace model

                Nothing ->
                    { model | pacePerKmSeconds = s }

        SetPacePerMileMinutes s ->
            case String.toInt s of
                Just minutes ->
                    let
                        seconds =
                            parseIntWithDefault model.pacePerMileSeconds
                    in
                    updatePace
                        (minutesAndSecondsPerMile minutes seconds)
                        model

                Nothing ->
                    { model | pacePerMileMinutes = s }

        SetPacePerMileSeconds s ->
            case String.toInt s of
                Just seconds ->
                    let
                        minutes =
                            parseIntWithDefault model.pacePerMileMinutes

                        pace =
                            if minutes == 0 then
                                minutesAndSecondsPerMile minutes (max seconds 0)

                            else
                                minutesAndSecondsPerMile minutes seconds
                    in
                    updatePace pace model

                Nothing ->
                    { model | pacePerMileSeconds = s }

        SetSpeedInKmh s ->
            case String.toFloat s of
                Just value ->
                    updateSpeed (kilometersPerHour value) model

                Nothing ->
                    { model | speedInKmh = s }

        SetSpeedInMph s ->
            case String.toFloat s of
                Just value ->
                    updateSpeed (milesPerHour value) model

                Nothing ->
                    { model | speedInMph = s }

        SetTotalTimeHours s ->
            case String.toInt s of
                Just hoursValue ->
                    let
                        minutesValue =
                            parseIntWithDefault model.durationMinutes

                        secondsValue =
                            parseIntWithDefault model.durationSeconds
                    in
                    updateDuration
                        (hoursMinutesAndSeconds
                            hoursValue
                            minutesValue
                            secondsValue
                        )
                        model

                Nothing ->
                    { model | durationHours = s }

        SetTotalTimeMinutes s ->
            case String.toInt s of
                Just minutesValue ->
                    let
                        hoursValue =
                            parseIntWithDefault model.durationHours

                        secondsValue =
                            parseIntWithDefault model.durationSeconds

                        duration =
                            if hoursValue == 0 then
                                hoursMinutesAndSeconds
                                    hoursValue
                                    (max minutesValue 0)
                                    secondsValue

                            else
                                hoursMinutesAndSeconds
                                    hoursValue
                                    minutesValue
                                    secondsValue
                    in
                    updateDuration duration model

                Nothing ->
                    { model | durationMinutes = s }

        SetTotalTimeSeconds s ->
            case String.toInt s of
                Just secondsValue ->
                    let
                        hoursValue =
                            parseIntWithDefault model.durationHours

                        minutesValue =
                            parseIntWithDefault model.durationMinutes

                        duration =
                            if hoursValue == 0 && minutesValue == 0 then
                                hoursMinutesAndSeconds
                                    hoursValue
                                    minutesValue
                                    (max secondsValue 0)

                            else
                                hoursMinutesAndSeconds
                                    hoursValue
                                    minutesValue
                                    secondsValue
                    in
                    updateDuration duration model

                Nothing ->
                    { model | durationSeconds = s }


updateDistance : Length -> Model -> Model
updateDistance distance model =
    let
        pace =
            minutesAndSecondsPerKilometer
                (parseIntWithDefault model.pacePerKmMinutes)
                (parseIntWithDefault model.pacePerKmSeconds)

        ( durationHours, durationMinutes, durationSeconds ) =
            durationFromDistanceAndPace distance pace
                |> inSeconds
                |> toHoursMinutesAndSeconds
    in
    { model
        | distanceInKilometers = roundForDisplay <| Length.inKilometers distance
        , distanceInMiles = roundForDisplay <| Length.inMiles distance
        , durationHours = String.fromInt durationHours
        , durationMinutes = String.fromInt durationMinutes
        , durationSeconds = String.fromInt durationSeconds
    }


updatePace : Pace -> Model -> Model
updatePace pace form =
    let
        ( pacePerKmMinutes, pacePerKmSeconds ) =
            inMinutesAndSecondsPerKilometer pace

        ( pacePerMileMinutes, pacePerMileSeconds ) =
            inMinutesAndSecondsPerMile pace

        speed =
            paceToSpeed pace

        distance =
            distanceFromKmString form.distanceInKilometers

        ( durationHours, durationMinutes, durationSeconds ) =
            durationFromDistanceAndPace distance pace
                |> inSeconds
                |> toHoursMinutesAndSeconds
    in
    { form
        | pacePerKmMinutes = String.fromInt pacePerKmMinutes
        , pacePerKmSeconds = String.fromInt pacePerKmSeconds
        , pacePerMileMinutes = String.fromInt pacePerMileMinutes
        , pacePerMileSeconds = String.fromInt pacePerMileSeconds
        , speedInKmh = roundForDisplay <| inKilometersPerHour speed
        , speedInMph = roundForDisplay <| inMilesPerHour speed
        , durationHours = String.fromInt durationHours
        , durationMinutes = String.fromInt durationMinutes
        , durationSeconds = String.fromInt durationSeconds
    }


updateSpeed : Speed -> Model -> Model
updateSpeed speed form =
    let
        pace =
            speedToPace speed

        ( pacePerKmMinutes, pacePerKmSeconds ) =
            inMinutesAndSecondsPerKilometer pace

        ( pacePerMileMinutes, pacePerMileSeconds ) =
            inMinutesAndSecondsPerMile pace

        distance =
            distanceFromKmString form.distanceInKilometers

        ( durationHours, durationMinutes, durationSeconds ) =
            durationFromDistanceAndPace distance pace
                |> inSeconds
                |> toHoursMinutesAndSeconds
    in
    { form
        | pacePerKmMinutes = String.fromInt pacePerKmMinutes
        , pacePerKmSeconds = String.fromInt pacePerKmSeconds
        , pacePerMileMinutes = String.fromInt pacePerMileMinutes
        , pacePerMileSeconds = String.fromInt pacePerMileSeconds
        , speedInKmh = roundForDisplay <| inKilometersPerHour speed
        , speedInMph = roundForDisplay <| inMilesPerHour speed
        , durationHours = String.fromInt durationHours
        , durationMinutes = String.fromInt durationMinutes
        , durationSeconds = String.fromInt durationSeconds
    }


updateDuration : Duration -> Model -> Model
updateDuration newTotalTime form =
    let
        ( durationHours, durationMinutes, durationSeconds ) =
            newTotalTime |> inSeconds |> toHoursMinutesAndSeconds

        distance =
            distanceFromKmString form.distanceInKilometers

        pace =
            paceFromDistanceAndDuration distance newTotalTime

        speed =
            paceToSpeed pace

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
        , speedInKmh = roundForDisplay <| inKilometersPerHour speed
        , speedInMph = roundForDisplay <| inMilesPerHour speed
        , durationHours = String.fromInt durationHours
        , durationMinutes = String.fromInt durationMinutes
        , durationSeconds = String.fromInt durationSeconds
    }


distanceFromKmString : String -> Length
distanceFromKmString s =
    s
        |> String.toFloat
        |> Maybe.withDefault 0
        |> Length.kilometers


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [ for "rpc-field-km" ] [ text "Distance in km" ]
            , input
                [ id "rpc-field-km"
                , type_ "number"
                , value model.distanceInKilometers
                , onInput SetKilometers
                , Attr.min "0"
                ]
                []
            ]
        , div []
            [ label [ for "rpc-field-miles" ] [ text "Distance in miles" ]
            , input
                [ id "rpc-field-miles"
                , type_ "number"
                , value model.distanceInMiles
                , onInput SetMiles
                , Attr.min "0"
                ]
                []
            ]
        , div []
            [ label [] [ text "Pace in min/km" ]
            , input
                [ type_ "number"
                , value model.pacePerKmMinutes
                , onInput SetPacePerKmMinutes
                , Attr.min "0"
                ]
                []
            , span [] [ text "m" ]
            , input
                [ type_ "number"
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
                , value model.pacePerMileMinutes
                , onInput SetPacePerMileMinutes
                , Attr.min "0"
                ]
                []
            , span [] [ text "m" ]
            , input
                [ type_ "number"
                , value model.pacePerMileSeconds
                , onInput SetPacePerMileSeconds
                ]
                []
            , span [] [ text "s" ]
            ]
        , div []
            [ label [] [ text "Speed in km/h" ]
            , input
                [ type_ "number"
                , value model.speedInKmh
                , onInput SetSpeedInKmh
                , Attr.min "0"
                ]
                []
            ]
        , div []
            [ label [] [ text "Speed in mi/h" ]
            , input
                [ type_ "number"
                , value model.speedInMph
                , onInput SetSpeedInMph
                , Attr.min "0"
                ]
                []
            ]
        , div []
            [ label [] [ text "Total Time" ]
            , input
                [ type_ "number"
                , value model.durationHours
                , onInput SetTotalTimeHours
                , Attr.min "0"
                ]
                []
            , span [] [ text "h" ]
            , input
                [ type_ "number"
                , value model.durationMinutes
                , onInput SetTotalTimeMinutes
                ]
                []
            , span [] [ text "m" ]
            , input
                [ type_ "number"
                , value model.durationSeconds
                , onInput SetTotalTimeSeconds
                ]
                []
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


inSecondsPerMeter : Pace -> Float
inSecondsPerMeter (Quantity.Quantity numSecondsPerMeter) =
    numSecondsPerMeter


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


paceFromDistanceAndDuration : Length -> Duration -> Pace
paceFromDistanceAndDuration distance newTotalTime =
    secondsPerMeter (inSeconds newTotalTime / inMeters distance)



-- Duration


durationFromDistanceAndPace : Length -> Pace -> Duration
durationFromDistanceAndPace distance pace =
    seconds <| inMeters distance * inSecondsPerMeter pace



-- Conversion


paceToSpeed : Pace -> Speed
paceToSpeed (Quantity.Quantity numSecondsPerMeter) =
    metersPerSecond (1 / numSecondsPerMeter)


speedToPace : Speed -> Pace
speedToPace (Quantity.Quantity numMetersPerSecond) =
    secondsPerMeter (1 / numMetersPerSecond)



-- Duration


hoursMinutesAndSeconds : Int -> Int -> Int -> Duration
hoursMinutesAndSeconds hoursValue minutesValue secondsValue =
    seconds <| toFloat (hoursValue * 3600 + minutesValue * 60 + secondsValue)


toMinutesAndSeconds : Float -> ( Int, Int )
toMinutesAndSeconds totalSecondsFloat =
    let
        totalSeconds =
            round totalSecondsFloat

        minutes =
            totalSeconds // 60

        seconds =
            totalSeconds - (minutes * 60)
    in
    ( minutes, seconds )


toHoursMinutesAndSeconds : Float -> ( Int, Int, Int )
toHoursMinutesAndSeconds totalSecondsFloat =
    let
        totalSeconds =
            round totalSecondsFloat

        hours =
            totalSeconds // 3600

        minutes =
            (totalSeconds - (3600 * hours)) // 60

        seconds =
            totalSeconds - (3600 * hours) - (minutes * 60)
    in
    ( hours, minutes, seconds )


mileInMeters : Float
mileInMeters =
    5280 * 12 * 0.0254


parseIntWithDefault : String -> Int
parseIntWithDefault s =
    s |> String.toInt |> Maybe.withDefault 0
