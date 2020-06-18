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
type alias EventHandler appMsg =
    Event.EventHandler appMsg


{-| Let the System decide what to do
-}
default : EventHandler appMsg
default =
    Event.Default


{-| Ignore the event
-}
ignore : EventHandler appMsg
ignore =
    Event.Ignore


{-| Before the default behaviour respond with a custom appMsg
-}
beforeDefault : appMsg -> EventHandler appMsg
beforeDefault =
    Event.BeforeDefault


{-| Ignore the default behaviour and respond with a custom appMsg
-}
custom : appMsg -> EventHandler appMsg
custom =
    Event.Custom



-- Component


{-| Components need to supply the following event handlers
-}
type alias ComponentEventHandlers appMsg =
    { onPIDNotFound : PID -> EventHandler appMsg
    , onStop : EventHandler appMsg
    }


{-| You can choose to apply a sytem default behaviour

It might be easy to start with systemDefault when specifying your components event handlers.

    { systemDefault | onStop = beforeDefault SaveProgress }

-}
systemDefault : ComponentEventHandlers appMsg
systemDefault =
    { onPIDNotFound = always default
    , onStop = default
    }


{-| You can choose to ignore all events
-}
ignoreAll : ComponentEventHandlers appMsg
ignoreAll =
    { onPIDNotFound = always ignore
    , onStop = ignore
    }
