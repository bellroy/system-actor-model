module System.Process exposing
    ( PID
    , equals, pidToInt, pidToString, pidSpawnedBy
    )

{-|


# Process ID

@docs PID


# Helpers

@docs equals, pidToInt, pidToString, pidSpawnedBy

-}

import System.Internal.PID as PID exposing (PID(..), equals, toInt)


{-| Each Process has an unique identifier ([PID](https://en.wikipedia.org/wiki/Process_identifier)).
-}
type alias PID =
    PID.PID


{-| Check if two PIDs are the same
-}
equals :
    PID
    -> PID
    -> Bool
equals =
    PID.equals


{-| Represent a PID as an Int
-}
pidToInt :
    PID
    -> Int
pidToInt =
    PID.toInt


{-| Represent a PID as a String
-}
pidToString :
    PID
    -> String
pidToString pid =
    case pid of
        PID.System ->
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
        PID.System ->
            pid

        PID.PID { spawnedBy } ->
            spawnedBy
