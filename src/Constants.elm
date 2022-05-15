module Constants exposing (halfMarathonLength, marathonLength, metersPerMile)

import Length exposing (Length)


metersPerMile : Float
metersPerMile =
    5280 * 12 * 0.0254


halfMarathonLength : Length
halfMarathonLength =
    Length.kilometers 21.0975


marathonLength : Length
marathonLength =
    Length.kilometers 42.195
