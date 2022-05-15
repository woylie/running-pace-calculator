module Distance exposing
    ( Distance(..)
    , distances
    , fromLength
    , fromString
    , toLength
    , toString
    )

import Constants
import Length exposing (Length)


type Distance
    = FiveKm
    | TenKm
    | FiveMi
    | TenMi
    | HalfMarathon
    | Marathon
    | Distance Length


distances : List Distance
distances =
    [ FiveKm, TenKm, FiveMi, TenMi, HalfMarathon, Marathon ]


fromString : String -> Maybe Distance
fromString s =
    if s == toString FiveKm then
        Just FiveKm

    else if s == toString FiveMi then
        Just FiveMi

    else if s == toString TenKm then
        Just TenKm

    else if s == toString TenMi then
        Just TenMi

    else if s == toString HalfMarathon then
        Just HalfMarathon

    else if s == toString Marathon then
        Just Marathon

    else
        Nothing


toString : Distance -> String
toString distance =
    case distance of
        FiveKm ->
            "5 km"

        FiveMi ->
            "5 mi"

        TenKm ->
            "10 km"

        TenMi ->
            "10 mi"

        HalfMarathon ->
            "half marathon"

        Marathon ->
            "marathon"

        Distance _ ->
            "other"


fromLength : Length -> Distance
fromLength length =
    let
        lengthInKilometers =
            Length.inKilometers length

        lengthInMiles =
            Length.inMiles length
    in
    if lengthInKilometers == 5 then
        FiveKm

    else if lengthInMiles == 5 then
        FiveMi

    else if lengthInKilometers == 10 then
        TenKm

    else if lengthInMiles == 10 then
        TenMi

    else if length == Constants.halfMarathonLength then
        HalfMarathon

    else if length == Constants.marathonLength then
        Marathon

    else
        Distance length


toLength : Distance -> Length
toLength distance =
    case distance of
        FiveKm ->
            Length.kilometers 5

        TenKm ->
            Length.kilometers 10

        FiveMi ->
            Length.miles 5

        TenMi ->
            Length.miles 10

        HalfMarathon ->
            Constants.halfMarathonLength

        Marathon ->
            Constants.marathonLength

        Distance length ->
            length
