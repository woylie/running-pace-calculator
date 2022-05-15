module Hms exposing
    ( hoursMinutesAndSeconds
    , toHoursMinutesAndSeconds
    , toMinutesAndSeconds
    )

import Duration exposing (Duration, seconds)


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
