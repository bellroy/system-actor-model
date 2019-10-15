module Actors.Snackbar exposing (Model, actor)

import ActorName
import Components.Snackbar as Snackbar
import Html exposing (Html)
import Msg as Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Ui exposing (toActor)
import System.Debug exposing (pidToString)
import System.Log exposing (toMeta)
import System.Message exposing (noOperation, populateView, spawn)
import System.Process exposing (PID)


type alias Model =
    Snackbar.Model


type alias MsgIn =
    Snackbar.MsgIn


type alias MsgOut =
    Snackbar.MsgOut


actor :
    (Snackbar.Model -> appModel)
    -> Actor Snackbar.Model appModel (Html Msg) Msg
actor wrapModel =
    toActor Snackbar.component
        { wrapModel = wrapModel
        , wrapMsg = Msg.Snackbar
        , mapIn = mapIn
        , mapOut = mapOut
        }


mapIn :
    Msg.AppMsg
    -> Maybe MsgIn
mapIn appMsg =
    case appMsg of
        Msg.Snackbar msgIn ->
            Just msgIn

        Msg.LogMsg logMessage ->
            let
                logMessageMeta =
                    toMeta logMessage

                logPid =
                    pidToString logMessageMeta.pid

                logDescription =
                    logMessageMeta.description
            in
            Just <| Snackbar.NewSnack { message = logDescription ++ " with PID: " ++ logPid }

        _ ->
            Nothing


mapOut :
    PID
    -> MsgOut
    -> Msg
mapOut pid msgOut =
    noOperation
