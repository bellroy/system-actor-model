module System.Internal.Event exposing
    ( Event(..)
    , EventHandler(..), mapEventHandler
    )

{-|


# Events

@docs Event

-}

import System.Internal.PID exposing (PID)


{-| A System Event
-}
type Event
    = OnPIDNotFound PID
    | OnKill


{-| A System Event Handler
-}
type EventHandler msgIn
    = Default
    | Ignore
    | BeforeDefault msgIn
    | Custom msgIn


{-| Map f over eventHandler
-}
mapEventHandler : (a -> b) -> EventHandler a -> EventHandler b
mapEventHandler f eventHandler =
    case eventHandler of
        Default ->
            Default

        Ignore ->
            Ignore

        BeforeDefault a ->
            BeforeDefault (f a)

        Custom a ->
            Custom (f a)
