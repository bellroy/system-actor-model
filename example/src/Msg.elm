module Msg exposing (AppMsg(..), Msg)

import ActorName exposing (ActorName)
import Address exposing (Address)
import Components.Counter as Counter
import Components.Counters as Counters
import Components.Snackbar as Snackbar
import Components.Templating as Templating
import System.Log exposing (LogMessage)
import System.Message exposing (SystemMessage)


type alias Msg =
    SystemMessage Address ActorName AppMsg


type AppMsg
    = Counters Counters.MsgIn
    | Counter Counter.MsgIn
    | Snackbar Snackbar.MsgIn
    | Templating Templating.MsgIn
    | LogMsg (LogMessage Address ActorName AppMsg)
