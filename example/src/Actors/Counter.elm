module Actors.Counter exposing (Model, actor)

import Components.Counter as Counter
import Html exposing (Html)
import Msg as Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Ui exposing (toActor)
import System.Log exposing (info)
import System.Message exposing (log, noOperation)
import System.Process exposing (PID)


type alias Model =
    Counter.Model


type alias MsgIn =
    Counter.MsgIn


type alias MsgOut =
    Counter.MsgOut


actor :
    (Model -> appModel)
    -> Actor Model appModel (Html Msg) Msg
actor wrapModel =
    toActor Counter.component
        { wrapModel = wrapModel
        , wrapMsg = Msg.Counter
        , mapIn = mapIn
        , mapOut = mapOut
        }


mapIn :
    Msg.AppMsg
    -> Maybe MsgIn
mapIn appMsg =
    case appMsg of
        Msg.Counter msgIn ->
            Just msgIn

        _ ->
            Nothing


mapOut :
    PID
    -> MsgOut
    -> Msg
mapOut pid msgOut =
    case msgOut of
        Counter.LogCreated ->
            log <| info pid "New Counter Created!"
