module Actors.Counters exposing (Model, actor)

import ActorName as ActorName exposing (ActorName(..))
import Components.Counters as Component
import Html exposing (Html)
import Msg as Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Layout exposing (toActor)
import System.Message exposing (batch, kill, sendToPid, spawn)
import System.Process exposing (PID)


type alias Model =
    Component.Model


type alias MsgIn =
    Component.MsgIn


type alias MsgOut =
    Component.MsgOut


actor :
    (Model -> appModel)
    -> Actor Model appModel (Html Msg) Msg
actor wrapModel =
    toActor Component.component
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
        Component.SpawnCounter ->
            spawn ActorName.Counter
                (sendToPid pid
                    << Msg.Counters
                    << Component.ReceiveCounter
                )

        Component.KillCounter killPid ->
            batch
                [ kill killPid
                , sendToPid pid <| Msg.Counters <| Component.KilledCounter killPid
                ]
