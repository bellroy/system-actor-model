module System.Internal.Component exposing (wrapEvents, wrapInit, wrapLayoutView, wrapSubscriptions, wrapToTuple, wrapUiView, wrapUpdate)

import Html as Html exposing (Html, map, text)
import Json.Decode as Decode
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Event exposing (Event(..), EventHandler, mapEventHandler)
import System.Internal.Message exposing (Control(..), Message(..))
import System.Internal.PID exposing (PID)
import System.Message exposing (sendToPid)


type alias Args a address actorName model actorModel msgIn msgOut wrappedMsg =
    { a
        | wrapModel : model -> actorModel
        , wrapMsg : msgIn -> wrappedMsg
        , mapIn : wrappedMsg -> Maybe msgIn
        , mapOut : PID -> msgOut -> Message address actorName wrappedMsg
    }


wrapInit :
    Args a address actorName model actorModel msgIn msgOut wrappedMsg
    -> (( PID, Decode.Value ) -> ( model, List msgOut, Cmd msgIn ))
    -> ( PID, Decode.Value )
    -> ( actorModel, Message address actorName wrappedMsg )
wrapInit args init ( pid, flags ) =
    init ( pid, flags )
        |> wrapToTuple args pid


wrapUpdate :
    Args a address actorName model actorModel msgIn msgOut wrappedMsg
    -> (msgIn -> model -> ( model, List msgOut, Cmd msgIn ))
    -> (model -> Message address actorName wrappedMsg -> PID -> ( actorModel, Message address actorName wrappedMsg ))
wrapUpdate args update model msg pid =
    case msg of
        ActorMsg wrappedMsg ->
            case args.mapIn wrappedMsg of
                Just msgIn ->
                    update msgIn model
                        |> wrapToTuple args pid

                Nothing ->
                    ( args.wrapModel model, UnmappedMsg wrappedMsg )

        _ ->
            ( args.wrapModel model, NoOp )


wrapSubscriptions :
    Args a address actorName model actorModel msgIn msgOut wrappedMsg
    -> (model -> Sub msgIn)
    -> model
    -> PID
    -> Sub (Message address actorName wrappedMsg)
wrapSubscriptions { wrapMsg } subs model pid =
    if subs model == Sub.none then
        Sub.none

    else
        Sub.map (sendToPid pid << wrapMsg) (subs model)


wrapEvents :
    Args a address actorName model actorModel msgIn msgOut wrappedMsg
    -> ComponentEventHandlers msgIn
    -> Event
    -> PID
    -> EventHandler (Message address actorName wrappedMsg)
wrapEvents { wrapMsg } { onPIDNotFound, onKill } event pid =
    case event of
        OnPIDNotFound pidNotFound ->
            mapEventHandler (sendToPid pid << wrapMsg) (onPIDNotFound pidNotFound)

        OnKill ->
            mapEventHandler (sendToPid pid << wrapMsg) onKill


wrapToTuple :
    Args a address actorName model actorModel msgIn msgOut wrappedMsg
    -> PID
    -> ( model, List msgOut, Cmd msgIn )
    -> ( actorModel, Message address actorName wrappedMsg )
wrapToTuple { wrapModel, wrapMsg, mapOut } pid ( model, msgsOut, cmd ) =
    let
        msgCmd =
            if cmd == Cmd.none then
                NoOp

            else
                Cmd.map
                    (sendToPid pid << wrapMsg)
                    cmd
                    |> Command
                    |> Control

        msg =
            (List.map (mapOut pid) msgsOut
                |> (::) msgCmd
            )
                |> Batch
                |> Control
                |> Context pid
    in
    ( wrapModel model
    , msg
    )


wrapLayoutView :
    Args a address actorName model actorModel msgIn msgOut wrappedMsg
    ->
        ((msgIn -> Message address actorName wrappedMsg)
         -> model
         -> (PID -> Html (Message address actorName wrappedMsg))
         -> Html (Message address actorName wrappedMsg)
        )
    -> model
    -> PID
    -> (PID -> Maybe (Html (Message address actorName wrappedMsg)))
    -> Html (Message address actorName wrappedMsg)
wrapLayoutView { wrapMsg } view model pid renderPID =
    view
        (sendToPid pid << wrapMsg)
        model
        (renderPID >> Maybe.withDefault (Html.text ""))


wrapUiView :
    Args a address actorName model appModel msgIn msgOut appMsg
    -> (model -> Html msgIn)
    -> model
    -> PID
    -> (PID -> Maybe output)
    -> Html (Message address actorName appMsg)
wrapUiView { wrapMsg } view model pid _ =
    view model
        |> Html.map (sendToPid pid << wrapMsg)
