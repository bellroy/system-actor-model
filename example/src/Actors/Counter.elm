module Actors.Counter exposing (Model, actor)

import Components.Counter as Component
import Html exposing (Html)
import Msg as Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Ui exposing (toActor)
import System.Message exposing (noOperation)
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
    noOperation
