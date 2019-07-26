module System.Internal.PID exposing
    ( PID(..)
    , equals
    , system
    , toInt
    )

{-| Each Process has an unique identifier ([PID](https://en.wikipedia.org/wiki/Process_identifier)).
-}


type PID
    = PID
        { id : Int
        , spawnedBy : PID
        }
    | System


{-| The System is represented by this PID.

It's represented as an Integer by 1.

-}
system : PID
system =
    System


{-| Compare two PIDs
-}
equals : PID -> PID -> Bool
equals a b =
    toInt a == toInt b


{-| Cast as int
-}
toInt : PID -> Int
toInt pid =
    case pid of
        System ->
            1

        PID { id } ->
            id
