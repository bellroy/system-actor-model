module System.Process exposing
    ( PID
    , equals, pidToInt, pidToString, pidSpawnedBy
    )

{-| A Process in the context of this package represents a spawned/active Actor.

Each Process gets assigned an unique identifier ([PID](https://en.wikipedia.org/wiki/Process_identifier)).
The PID also holds information about who spawned, or started, the process.


# ProcessIDentifier

@docs PID


# Helpers

@docs equals, pidToInt, pidToString, pidSpawnedBy

-}

import System.Internal.PID as PID


{-| -}
type alias PID =
    PID.PID


{-| Check if two PIDs are identical.
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
pidToString =
    PID.toString


{-| Get the PID responsible for spawing the given PID
-}
pidSpawnedBy :
    PID
    -> PID
pidSpawnedBy =
    PID.toSpawnedBy
