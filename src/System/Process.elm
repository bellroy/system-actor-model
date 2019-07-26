module System.Process exposing
    ( PID
    , equals
    )

{-|


# Process ID

@docs PID


# Helpers

@docs equals

-}

import System.Internal.PID as PID exposing (PID, equals)


{-| Each Process has an unique identifier ([PID](https://en.wikipedia.org/wiki/Process_identifier)).
-}
type alias PID =
    PID.PID


{-| Check if two PID's are the same
-}
equals :
    PID
    -> PID
    -> Bool
equals =
    PID.equals
