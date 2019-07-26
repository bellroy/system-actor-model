module System.Event exposing
    ( EventHandler, default, ignore, beforeDefault, custom
    , ComponentEventHandlers, systemDefault, ignoreAll
    )

{-|


# Event Handling

@docs EventHandler, default, ignore, beforeDefault, custom


# Component

Your components can receive Events that are being spawned by the System.
It's up to your component to handle these.

@docs ComponentEventHandlers, systemDefault, ignoreAll

-}

import System.Internal.Event as Event exposing (EventHandler(..))
import System.Internal.PID exposing (PID)



-- Event Handling


{-| A System Event Handler
-}
type alias EventHandler msgIn =
    Event.EventHandler msgIn


{-| Let the System decide what to do
-}
default : EventHandler msgIn
default =
    Event.Default


{-| Ignore the event
-}
ignore : EventHandler msgIn
ignore =
    Event.Ignore


{-| Before the default behaviour respond with a custom msgIn
-}
beforeDefault : msgIn -> EventHandler msgIn
beforeDefault =
    Event.BeforeDefault


{-| Ignore the default behaviour and respond with a custom msgIn
-}
custom : msgIn -> EventHandler msgIn
custom =
    Event.Custom



-- Component


{-| Components need to supply the following event handlers
-}
type alias ComponentEventHandlers msgIn =
    { onPIDNotFound : PID -> EventHandler msgIn
    , onKill : EventHandler msgIn
    }


{-| You can choose to apply a sytem default behaviour

It might be easy to start with systemDefault when specifying your components event handlers.

    { systemDefault | onKill = beforeDefault SaveProgress }

-}
systemDefault : ComponentEventHandlers msgIn
systemDefault =
    { onPIDNotFound = always default
    , onKill = default
    }


{-| You can choose to ignore all events
-}
ignoreAll : ComponentEventHandlers msgIn
ignoreAll =
    { onPIDNotFound = always ignore
    , onKill = ignore
    }
