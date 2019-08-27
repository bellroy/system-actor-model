module System.Actor exposing (Actor, toSystemActor)

{-|


# Actor

In the context of this package an Actor is a Component that is mapped to the System.

Currently there are three components defined;

  - The Worker Component
  - The Ui Component
  - The Layout Component

An Actor probably looks very familiar to you,
that is because it closely resembles the Elm Program definition.

It should be relativaly straight forwards to map you exisiting Elm component on top
of one of the existing Components.

Actors can communicate directly with other Actors only when it knows it's PID.

@docs Actor, toSystemActor

-}

import Json.Decode as Decode
import System.Event exposing (EventHandler)
import System.Internal.Event exposing (Event)
import System.Internal.PID exposing (PID)
import System.Internal.SystemActor exposing (SystemActor(..))


{-| The type of an Actor
-}
type alias Actor model actorModel output msg =
    { init : ( PID, Decode.Value ) -> ( actorModel, msg )
    , update : model -> msg -> PID -> ( actorModel, msg )
    , view : model -> PID -> (PID -> Maybe output) -> output
    , subscriptions : model -> PID -> Sub msg
    , events : Event -> PID -> EventHandler msg
    }


{-| Apply your model over your Actor to create a SystemActor
-}
toSystemActor :
    Actor model actorModel output msg
    -> model
    -> SystemActor actorModel output msg
toSystemActor actor model =
    SystemActor
        { init = actor.init
        , update = actor.update model
        , view = actor.view model
        , subscriptions = actor.subscriptions model
        , events = actor.events
        }
