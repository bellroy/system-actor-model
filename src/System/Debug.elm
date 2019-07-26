module System.Debug exposing (pidToInt, pidToString, pidSpawnedBy)

{-|


# Processes

@docs pidToInt, pidToString, pidSpawnedBy

-}

import System.Internal.PID exposing (PID(..), toInt)


{-| Represent a PID as an Int
-}
pidToInt :
    PID
    -> Int
pidToInt =
    toInt


{-| Represent a PID as a String
-}
pidToString :
    PID
    -> String
pidToString pid =
    case pid of
        System ->
            "System"

        _ ->
            String.fromInt <| pidToInt pid


{-| Get the PID responsible for spawing the given PID
-}
pidSpawnedBy :
    PID
    -> PID
pidSpawnedBy pid =
    case pid of
        System ->
            pid

        PID { spawnedBy } ->
            spawnedBy
