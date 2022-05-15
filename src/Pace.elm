module Pace exposing
    ( Pace
    , durationFromDistanceAndPace
    , fromSpeed
    , inMinutesAndSecondsPerKilometer
    , inMinutesAndSecondsPerMile
    , minutesAndSecondsPerKilometer
    , minutesAndSecondsPerMile
    , minutesPerKilometer
    , paceFromDistanceAndDuration
    , toSpeed
    )

import Constants
import Duration exposing (Duration, Seconds, inSeconds, seconds)
import Hms
import Length exposing (Length, Meters, inMeters)
import Quantity exposing (Quantity, Rate)
import Speed exposing (Speed, metersPerSecond)


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
    pace |> inSecondsPerKilometer |> Hms.toMinutesAndSeconds


inSecondsPerMile : Pace -> Float
inSecondsPerMile (Quantity.Quantity numSecondsPerMeter) =
    numSecondsPerMeter * Constants.metersPerMile


minutesAndSecondsPerMile : Int -> Int -> Pace
minutesAndSecondsPerMile minutes seconds =
    secondsPerMeter
        ((toFloat minutes * 60 + toFloat seconds) / Constants.metersPerMile)


inMinutesAndSecondsPerMile : Pace -> ( Int, Int )
inMinutesAndSecondsPerMile pace =
    pace |> inSecondsPerMile |> Hms.toMinutesAndSeconds


paceFromDistanceAndDuration : Length -> Duration -> Pace
paceFromDistanceAndDuration distance newTotalTime =
    secondsPerMeter (inSeconds newTotalTime / inMeters distance)


durationFromDistanceAndPace : Length -> Pace -> Duration
durationFromDistanceAndPace distance pace =
    seconds <| inMeters distance * inSecondsPerMeter pace


toSpeed : Pace -> Speed
toSpeed (Quantity.Quantity numSecondsPerMeter) =
    metersPerSecond (1 / numSecondsPerMeter)


fromSpeed : Speed -> Pace
fromSpeed (Quantity.Quantity numMetersPerSecond) =
    secondsPerMeter (1 / numMetersPerSecond)
