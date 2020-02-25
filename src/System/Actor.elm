module System.Actor exposing
    ( Actor
    , SystemActor, toSystemActor
    )

{-|


# Actor

In the context of this package an Actor is a Component that is mapped to the System.

Currently there are three components defined;

  - The Layout Component
  - The Ui Component
  - The Worker Component

An Actor probably looks very familiar to you, that is because it closely resembles the Elm Program definition.

It should be relativaly straight forwards to map you exisiting Elm component on top of one of the existing Components.

Actors can communicate with other Actors when they know their PIDs or they can send a message to other Actors listening on a given Address.


## Type Prefixes

when a type variable is prefixed with

  - `component*` Your component should provide this type
  - `application*` Your application should provide this type
  - `system*` The system will provide this type


## Actor

An Actor is an alias for the following Record

@docs Actor


## SystemActor

@docs SystemActor, toSystemActor

-}

import Json.Decode exposing (Value)
import System.Event exposing (EventHandler)
import System.Internal.Event exposing (Event)
import System.Internal.PID exposing (PID)
import System.Internal.SystemActor as Internal


{-| An Actor looks a lot like a Browser.element!
Therefore it is quite easy to grab an existing Elm application and make it part of your new System Actor Model powered application.
-}
type alias Actor componentModel applicationModel componentOutput componentMsgIn =
    { init : ( PID, Value ) -> ( applicationModel, componentMsgIn )
    , update : componentModel -> componentMsgIn -> PID -> ( applicationModel, componentMsgIn )
    , view : componentModel -> PID -> (PID -> Maybe componentOutput) -> componentOutput
    , subscriptions : componentModel -> PID -> Sub componentMsgIn
    , events : Event -> PID -> EventHandler componentMsgIn
    }


{-| An Actor within the System has a different Type,
it no longer has the `componentModel` in the type definition, this is because the `componentModel` is no wrapped using the `applicationModel`.

You can create a SystemActor using the `toSystemActor` function.

-}
type alias SystemActor applicationModel componentOutput componentMsgIn =
    Internal.SystemActor applicationModel componentOutput componentMsgIn


{-| Apply your model over your Actor and create a SystemActor
-}
toSystemActor :
    Actor componentModel applicationModel componentOutput componentMsgIn
    -> componentModel
    -> SystemActor applicationModel componentOutput componentMsgIn
toSystemActor actor componentModel =
    Internal.SystemActor
        { init = actor.init
        , update = actor.update componentModel
        , view = actor.view componentModel
        , subscriptions = actor.subscriptions componentModel
        , events = actor.events
        }
