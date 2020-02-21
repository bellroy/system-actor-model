module System.Internal.Component exposing (wrapEvents, wrapInit, wrapLayoutView, wrapSubscriptions, wrapToTuple, wrapUiView, wrapUpdate)

import Html as Html exposing (Html, map, text)
import Json.Decode as Decode
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Event exposing (Event(..), EventHandler, mapEventHandler)
import System.Internal.Message exposing (Control(..), Message(..))
import System.Internal.PID exposing (PID)
import System.Message exposing (sendToPid)


type alias Args a address actorName model actorModel msgIn msgOut appMsg =
    { a
        | wrapModel : model -> actorModel
        , wrapMsg : msgIn -> appMsg
        , mapIn : appMsg -> Maybe msgIn
        , mapOut : PID -> msgOut -> Message address actorName appMsg
    }


wrapInit :
    Args a address actorName model actorModel msgIn msgOut appMsg
    ->
        (( PID, Decode.Value )
         -> ( model, List msgOut, Cmd msgIn )
        )
    -> ( PID, Decode.Value )
    -> ( actorModel, Message address actorName appMsg )
wrapInit args init ( pid, flags ) =
    init ( pid, flags )
        |> wrapToTuple args pid


wrapUpdate :
    Args a address actorName model actorModel msgIn msgOut appMsg
    ->
        (msgIn
         -> model
         -> ( model, List msgOut, Cmd msgIn )
        )
    -> model
    -> Message address actorName appMsg
    -> PID
    -> ( actorModel, Message address actorName appMsg )
wrapUpdate args update model msg pid =
    case msg of
        ActorMsg appMsg ->
            case args.mapIn appMsg of
                Just msgIn ->
                    update msgIn model
                        |> wrapToTuple args pid

                Nothing ->
                    ( args.wrapModel model, UnmappedMsg appMsg )

        _ ->
            ( args.wrapModel model, NoOp )


wrapSubscriptions :
    Args a address actorName model actorModel msgIn msgOut appMsg
    -> (model -> Sub msgIn)
    -> model
    -> PID
    -> Sub (Message address actorName appMsg)
wrapSubscriptions { wrapMsg } subs model pid =
    if subs model == Sub.none then
        Sub.none

    else
        Sub.map (sendToPid pid << wrapMsg) (subs model)


wrapEvents :
    Args a address actorName model actorModel msgIn msgOut appMsg
    -> ComponentEventHandlers msgIn
    -> Event
    -> PID
    -> EventHandler (Message address actorName appMsg)
wrapEvents { wrapMsg } { onPIDNotFound, onKill } event pid =
    case event of
        OnPIDNotFound pidNotFound ->
            mapEventHandler (sendToPid pid << wrapMsg) (onPIDNotFound pidNotFound)

        OnKill ->
            mapEventHandler (sendToPid pid << wrapMsg) onKill


wrapToTuple :
    Args a address actorName model actorModel msgIn msgOut appMsg
    -> PID
    -> ( model, List msgOut, Cmd msgIn )
    -> ( actorModel, Message address actorName appMsg )
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
    Args a address actorName model actorModel msgIn msgOut appMsg
    ->
        ((msgIn -> Message address actorName appMsg)
         -> model
         -> (PID -> Html (Message address actorName appMsg))
         -> Html (Message address actorName appMsg)
        )
    -> model
    -> PID
    -> (PID -> Maybe (Html (Message address actorName appMsg)))
    -> Html (Message address actorName appMsg)
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
