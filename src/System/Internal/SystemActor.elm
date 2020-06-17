module System.Internal.SystemActor exposing (SystemActor(..))

import Json.Decode exposing (Value)
import System.Event exposing (EventHandler)
import System.Internal.Event exposing (Event)
import System.Internal.PID exposing (PID)


type SystemActor appModel output componentMsgIn
    = SystemActor
        { init : ( PID, Value ) -> ( appModel, componentMsgIn )
        , update : componentMsgIn -> PID -> ( appModel, componentMsgIn )
        , view : Maybe (PID -> (PID -> Maybe output) -> output)
        , subscriptions : PID -> Sub componentMsgIn
        , events : Event -> PID -> EventHandler componentMsgIn
        }
