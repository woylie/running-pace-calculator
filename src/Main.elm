module Main exposing (Msg(..), main, update, view)

import Browser
import Distance exposing (Distance(..))
import Duration exposing (Duration, inSeconds)
import Hms
import Html exposing (Html, div, input, label, option, select, span, text)
import Html.Attributes as Attr exposing (for, id, selected, type_, value)
import Html.Events exposing (onInput)
import Length exposing (Length, inKilometers, inMiles, kilometers, miles)
import Pace exposing (Pace, paceFromDistanceAndDuration)
import Round
import Speed
    exposing
        ( Speed
        , inKilometersPerHour
        , inMilesPerHour
        , kilometersPerHour
        , milesPerHour
        )


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { distanceInKilometers : String
    , distanceInMiles : String
    , distanceSelected : Distance
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
    | SetDistance (Maybe Distance)
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
            Pace.minutesPerKilometer 6
    in
    emptyModel
        |> updateDistance distance
        |> updatePace pace


emptyModel : Model
emptyModel =
    { distanceInKilometers = ""
    , distanceInMiles = ""
    , distanceSelected = Distance (Length.kilometers 0)
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
        SetDistance maybeDistance ->
            case maybeDistance of
                Just distance ->
                    updateDistance (Distance.toLength distance) model

                Nothing ->
                    let
                        distanceSelected =
                            model.distanceInKilometers
                                |> String.toFloat
                                |> Maybe.withDefault 0
                                |> Length.kilometers
                                |> Distance
                    in
                    { model | distanceSelected = distanceSelected }

        SetKilometers s ->
            case String.toFloat s of
                Just km ->
                    let
                        newModel =
                            updateDistance (Length.kilometers (max km 0)) model
                    in
                    { newModel | distanceInKilometers = s }

                Nothing ->
                    { model | distanceInKilometers = s }

        SetMiles s ->
            case String.toFloat s of
                Just mi ->
                    let
                        newModel =
                            updateDistance (Length.miles mi) model
                    in
                    { newModel | distanceInMiles = s }

                Nothing ->
                    { model | distanceInMiles = s }

        SetPacePerKmMinutes s ->
            case String.toInt s of
                Just minutes ->
                    let
                        seconds =
                            toIntWithDefault model.pacePerKmSeconds
                    in
                    updatePace
                        (Pace.minutesAndSecondsPerKilometer
                            (max minutes 0)
                            seconds
                        )
                        model

                Nothing ->
                    { model | pacePerKmMinutes = s }

        SetPacePerKmSeconds s ->
            case String.toInt s of
                Just seconds ->
                    let
                        minutes =
                            toIntWithDefault model.pacePerKmMinutes

                        pace =
                            if minutes == 0 then
                                Pace.minutesAndSecondsPerKilometer
                                    minutes
                                    (max seconds 0)

                            else
                                Pace.minutesAndSecondsPerKilometer
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
                            toIntWithDefault model.pacePerMileSeconds
                    in
                    updatePace
                        (Pace.minutesAndSecondsPerMile (max minutes 0) seconds)
                        model

                Nothing ->
                    { model | pacePerMileMinutes = s }

        SetPacePerMileSeconds s ->
            case String.toInt s of
                Just seconds ->
                    let
                        minutes =
                            toIntWithDefault model.pacePerMileMinutes

                        pace =
                            if minutes == 0 then
                                Pace.minutesAndSecondsPerMile
                                    minutes
                                    (max seconds 0)

                            else
                                Pace.minutesAndSecondsPerMile
                                    minutes
                                    seconds
                    in
                    updatePace pace model

                Nothing ->
                    { model | pacePerMileSeconds = s }

        SetSpeedInKmh s ->
            case String.toFloat s of
                Just value ->
                    let
                        newModel =
                            updateSpeed (kilometersPerHour value) model
                    in
                    { newModel | speedInKmh = s }

                Nothing ->
                    { model | speedInKmh = s }

        SetSpeedInMph s ->
            case String.toFloat s of
                Just value ->
                    let
                        newModel =
                            updateSpeed (milesPerHour value) model
                    in
                    { newModel | speedInMph = s }

                Nothing ->
                    { model | speedInMph = s }

        SetTotalTimeHours s ->
            case String.toInt s of
                Just hoursValue ->
                    let
                        minutesValue =
                            toIntWithDefault model.durationMinutes

                        secondsValue =
                            toIntWithDefault model.durationSeconds
                    in
                    updateDuration
                        (Hms.hoursMinutesAndSeconds
                            (max hoursValue 0)
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
                            toIntWithDefault model.durationHours

                        secondsValue =
                            toIntWithDefault model.durationSeconds

                        duration =
                            if hoursValue == 0 then
                                Hms.hoursMinutesAndSeconds
                                    hoursValue
                                    (max minutesValue 0)
                                    secondsValue

                            else
                                Hms.hoursMinutesAndSeconds
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
                            toIntWithDefault model.durationHours

                        minutesValue =
                            toIntWithDefault model.durationMinutes

                        duration =
                            if hoursValue == 0 && minutesValue == 0 then
                                Hms.hoursMinutesAndSeconds
                                    hoursValue
                                    minutesValue
                                    (max secondsValue 0)

                            else
                                Hms.hoursMinutesAndSeconds
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
            Pace.minutesAndSecondsPerKilometer
                (toIntWithDefault model.pacePerKmMinutes)
                (toIntWithDefault model.pacePerKmSeconds)

        ( durationHours, durationMinutes, durationSeconds ) =
            Pace.durationFromDistanceAndPace distance pace
                |> inSeconds
                |> Hms.toHoursMinutesAndSeconds

        distanceInKilometers =
            Length.inKilometers distance

        distanceInMiles =
            Length.inMiles distance
    in
    { model
        | distanceInKilometers = roundForDisplay <| distanceInKilometers
        , distanceInMiles = roundForDisplay <| distanceInMiles
        , distanceSelected = Distance.fromLength distance
        , durationHours = String.fromInt durationHours
        , durationMinutes = String.fromInt durationMinutes
        , durationSeconds = String.fromInt durationSeconds
    }


updatePace : Pace -> Model -> Model
updatePace pace model =
    let
        ( pacePerKmMinutes, pacePerKmSeconds ) =
            Pace.inMinutesAndSecondsPerKilometer pace

        ( pacePerMileMinutes, pacePerMileSeconds ) =
            Pace.inMinutesAndSecondsPerMile pace

        speed =
            Pace.toSpeed pace

        distance =
            Distance.toLength model.distanceSelected

        ( durationHours, durationMinutes, durationSeconds ) =
            Pace.durationFromDistanceAndPace distance pace
                |> inSeconds
                |> Hms.toHoursMinutesAndSeconds
    in
    { model
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
updateSpeed speed model =
    let
        pace =
            Pace.fromSpeed speed

        ( pacePerKmMinutes, pacePerKmSeconds ) =
            Pace.inMinutesAndSecondsPerKilometer pace

        ( pacePerMileMinutes, pacePerMileSeconds ) =
            Pace.inMinutesAndSecondsPerMile pace

        distance =
            Distance.toLength model.distanceSelected

        ( durationHours, durationMinutes, durationSeconds ) =
            Pace.durationFromDistanceAndPace distance pace
                |> inSeconds
                |> Hms.toHoursMinutesAndSeconds
    in
    { model
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
updateDuration newTotalTime model =
    let
        ( durationHours, durationMinutes, durationSeconds ) =
            newTotalTime |> inSeconds |> Hms.toHoursMinutesAndSeconds

        distance =
            Distance.toLength model.distanceSelected

        pace =
            paceFromDistanceAndDuration distance newTotalTime

        speed =
            Pace.toSpeed pace

        ( pacePerKmMinutes, pacePerKmSeconds ) =
            Pace.inMinutesAndSecondsPerKilometer pace

        ( pacePerMileMinutes, pacePerMileSeconds ) =
            Pace.inMinutesAndSecondsPerMile pace
    in
    { model
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
            [ label [] [ text "Distance" ]
            , distanceSelect model.distanceSelected
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


distanceSelect : Distance -> Html Msg
distanceSelect distanceSelected =
    let
        distanceOption : Distance -> Html Msg
        distanceOption distance =
            option
                [ selected (distanceSelected == distance)
                , value <| Distance.toString distance
                ]
                [ text <| Distance.toString distance ]
    in
    select [ onInput (SetDistance << Distance.fromString) ] <|
        [ option [] [] ]
            ++ List.map distanceOption Distance.distances


roundForDisplay : Float -> String
roundForDisplay n =
    let
        precision =
            4

        roundedN =
            n
                |> Round.round precision
                |> String.trim

        decimals =
            String.right precision roundedN

        whole =
            String.slice 0 (-1 * (precision + 1)) roundedN

        newDecimals =
            removeTrailingZeros decimals
    in
    if newDecimals == "" then
        whole

    else
        whole ++ "." ++ newDecimals


removeTrailingZeros : String -> String
removeTrailingZeros s =
    let
        trim : Char -> String -> String
        trim ch acc =
            if acc == "" && ch == '0' then
                ""

            else
                String.fromChar ch ++ acc
    in
    String.foldr trim "" s


toIntWithDefault : String -> Int
toIntWithDefault s =
    s |> String.toInt |> Maybe.withDefault 0
