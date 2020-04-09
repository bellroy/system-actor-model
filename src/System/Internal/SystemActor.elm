module System.Internal.SystemActor exposing (SystemActor(..))

import Json.Decode exposing (Value)
import System.Event exposing (EventHandler)
import System.Internal.Event exposing (Event)
import System.Internal.PID exposing (PID)


type SystemActor applicationModel componentOutput componentMsgIn
    = SystemActor
        { init : ( PID, Value ) -> ( applicationModel, componentMsgIn )
        , update : componentMsgIn -> PID -> ( applicationModel, componentMsgIn )
        , view : Maybe (PID -> (PID -> Maybe componentOutput) -> componentOutput)
        , subscriptions : PID -> Sub componentMsgIn
        , events : Event -> PID -> EventHandler componentMsgIn
        }
