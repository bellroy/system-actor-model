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

It's quite easy to grab an existing Elm application and make it part of an application that is setup to use this package.

-}
type alias Actor componentModel appModel output systemMsg =
    { init : ( PID, Value ) -> ( appModel, systemMsg )
    , update : componentModel -> systemMsg -> PID -> ( appModel, systemMsg )
    , subscriptions : componentModel -> PID -> Sub systemMsg
    , events : Event -> PID -> EventHandler systemMsg
    , view : Maybe (componentModel -> PID -> (PID -> Maybe output) -> output)
    }


{-| An Actor within the System has a different Type,

it no longer has the `componentModel` in the type definition, this is because the `componentModel` is no wrapped using the `appModel`.

You can create a SystemActor using the `toSystemActor` function.

-}
type alias SystemActor appModel output systemMsg =
    Internal.SystemActor appModel output systemMsg


{-| Apply your model over your Actor and create a SystemActor
-}
toSystemActor :
    Actor componentModel appModel output systemMsg
    -> componentModel
    -> SystemActor appModel output systemMsg
toSystemActor actor componentModel =
    Internal.SystemActor
        { init = actor.init
        , update = actor.update componentModel
        , view = Maybe.map (\v -> v componentModel) actor.view
        , subscriptions = actor.subscriptions componentModel
        , events = actor.events
        }
