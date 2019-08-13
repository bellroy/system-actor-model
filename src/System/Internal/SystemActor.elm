module System.Internal.SystemActor exposing (SystemActor(..))

import Json.Decode as Decode
import System.Event exposing (EventHandler)
import System.Internal.Event exposing (Event)
import System.Internal.PID exposing (PID)


type SystemActor actorModel output msg
    = SystemActor
        { init : ( PID, Decode.Value ) -> ( actorModel, msg )
        , update : msg -> PID -> ( actorModel, msg )
        , view : PID -> (PID -> Maybe output) -> output
        , subscriptions : PID -> Sub msg
        , events : Event -> PID -> EventHandler msg
        }
