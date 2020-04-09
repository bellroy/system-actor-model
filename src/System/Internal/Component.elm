module System.Internal.Component exposing (wrapEvents, wrapInit, wrapLayoutView, wrapSubscriptions, wrapToTuple, wrapUiView, wrapUpdate)

import Json.Decode as Decode
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Event exposing (Event(..), EventHandler, mapEventHandler)
import System.Internal.Message exposing (Control(..), SystemMessage(..))
import System.Internal.PID exposing (PID)
import System.Message exposing (sendToPid)


type alias Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage =
    { a
        | wrapModel : applicationModel -> componentModel
        , wrapMsg : componentMsgIn -> applicationMessage
        , mapIn : applicationMessage -> Maybe componentMsgIn
        , mapOut : PID -> componentMsgOut -> SystemMessage applicationAddress applicationActorName applicationMessage
    }


wrapInit :
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    ->
        (( PID, Decode.Value )
         -> ( applicationModel, List componentMsgOut, Cmd componentMsgIn )
        )
    -> ( PID, Decode.Value )
    -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
wrapInit args init ( pid, flags ) =
    init ( pid, flags )
        |> wrapToTuple args pid


wrapUpdate :
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    ->
        (componentMsgIn
         -> applicationModel
         -> ( applicationModel, List componentMsgOut, Cmd componentMsgIn )
        )
    -> applicationModel
    -> SystemMessage applicationAddress applicationActorName applicationMessage
    -> PID
    -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
wrapUpdate args update applicationModel msg pid =
    case msg of
        ActorMsg applicationMessage ->
            case args.mapIn applicationMessage of
                Just componentMsgIn ->
                    update componentMsgIn applicationModel
                        |> wrapToTuple args pid

                Nothing ->
                    ( args.wrapModel applicationModel, UnmappedMsg applicationMessage )

        _ ->
            ( args.wrapModel applicationModel, NoOp )


wrapSubscriptions :
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    -> (applicationModel -> Sub componentMsgIn)
    -> applicationModel
    -> PID
    -> Sub (SystemMessage applicationAddress applicationActorName applicationMessage)
wrapSubscriptions { wrapMsg } subs applicationModel pid =
    if subs applicationModel == Sub.none then
        Sub.none

    else
        Sub.map (sendToPid pid << wrapMsg) (subs applicationModel)


wrapEvents :
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    -> ComponentEventHandlers componentMsgIn
    -> Event
    -> PID
    -> EventHandler (SystemMessage applicationAddress applicationActorName applicationMessage)
wrapEvents { wrapMsg } { onPIDNotFound, onKill } event pid =
    case event of
        OnPIDNotFound pidNotFound ->
            mapEventHandler (sendToPid pid << wrapMsg) (onPIDNotFound pidNotFound)

        OnKill ->
            mapEventHandler (sendToPid pid << wrapMsg) onKill


wrapToTuple :
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    -> PID
    -> ( applicationModel, List componentMsgOut, Cmd componentMsgIn )
    -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
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
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    ->
        ((componentMsgIn -> SystemMessage applicationAddress applicationActorName applicationMessage)
         -> applicationModel
         -> (PID -> Maybe componentOutput)
         -> componentOutput
        )
    -> applicationModel
    -> PID
    -> (PID -> Maybe componentOutput)
    -> componentOutput
wrapLayoutView { wrapMsg } view model pid renderPid =
    view
        (sendToPid pid << wrapMsg)
        model
        renderPid


wrapUiView :
    Args a applicationAddress applicationActorName applicationModel componentModel componentMsgIn componentMsgOut applicationMessage
    ->
        ((componentMsgIn -> SystemMessage applicationAddress applicationActorName applicationMessage)
         -> applicationModel
         -> componentOutput
        )
    -> applicationModel
    -> PID
    -> (PID -> Maybe componentOutput)
    -> componentOutput
wrapUiView { wrapMsg } view model pid _ =
    view
        (sendToPid pid << wrapMsg)
        model
