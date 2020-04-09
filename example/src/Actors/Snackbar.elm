module Actors.Snackbar exposing (Model, actor)

import Components.Snackbar as Snackbar
import Html exposing (Html)
import Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Ui as Ui
import System.Log as Log
import System.Message as SystemMessage
import System.Process as PID


type alias Model =
    Snackbar.Model


type alias MsgIn =
    Snackbar.MsgIn


actor :
    (Snackbar.Model -> appModel)
    -> Actor Snackbar.Model appModel (Html Msg) Msg
actor wrapModel =
    Ui.toActor Snackbar.component
        { wrapModel = wrapModel
        , wrapMsg = Msg.Snackbar
        , mapIn = mapIn
        , mapOut = mapOut
        }


mapIn : Msg.AppMsg -> Maybe MsgIn
mapIn appMsg =
    case appMsg of
        Msg.Snackbar msgIn ->
            Just msgIn

        Msg.LogMsg logMessage ->
            let
                logMessageMeta =
                    Log.toMeta logMessage

                logPid =
                    PID.pidToString logMessageMeta.pid

                logDescription =
                    logMessageMeta.description
            in
            Snackbar.NewSnackMessage (logDescription ++ " with PID: " ++ logPid)
                |> Just

        _ ->
            Nothing


mapOut : a -> b -> Msg
mapOut _ _ =
    SystemMessage.noOperation
