module Actors.Counters exposing (Model, actor)

import ActorName as ActorName exposing (ActorName(..))
import Components.Counters as Counters
import Html exposing (Html)
import Json.Encode as Encode
import Msg as Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Layout exposing (toActor)
import System.Message exposing (batch, kill, sendToPid, spawnWithFlags)
import System.Process exposing (PID)


type alias Model =
    Counters.Model


type alias MsgIn =
    Counters.MsgIn


type alias MsgOut =
    Counters.MsgOut


actor :
    (Model -> appModel)
    -> Actor Model appModel (Html Msg) Msg
actor wrapModel =
    toActor Counters.component
        { wrapModel = wrapModel
        , wrapMsg = Msg.Counters
        , mapIn = mapIn
        , mapOut = mapOut
        }


mapIn :
    Msg.AppMsg
    -> Maybe MsgIn
mapIn appMsg =
    case appMsg of
        Msg.Counters msgIn ->
            Just msgIn

        _ ->
            Nothing


mapOut :
    PID
    -> MsgOut
    -> Msg
mapOut pid msgOut =
    case msgOut of
        Counters.SpawnCounter intialValue ->
            spawnWithFlags
                (Encode.int intialValue)
                ActorName.Counter
                (sendToPid pid
                    << Msg.Counters
                    << Counters.ReceiveCounter
                )

        Counters.KillCounter killPid ->
            batch
                [ kill killPid
                , sendToPid pid <| Msg.Counters <| Counters.KilledCounter killPid
                ]
