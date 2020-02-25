module System.Internal.PID exposing
    ( PID(..)
    , equals
    , system
    , toInt
    , toSpawnedBy
    , toString
    )


type PID
    = PID
        { id : Int
        , spawnedBy : PID
        }
    | System


system : PID
system =
    System


equals : PID -> PID -> Bool
equals a b =
    toInt a == toInt b


toInt : PID -> Int
toInt pid =
    case pid of
        System ->
            1

        PID { id } ->
            id


toString : PID -> String
toString pid =
    case pid of
        System ->
            "System"

        _ ->
            String.fromInt <| toInt pid


toSpawnedBy :
    PID
    -> PID
toSpawnedBy pid =
    case pid of
        System ->
            pid

        PID { spawnedBy } ->
            spawnedBy
