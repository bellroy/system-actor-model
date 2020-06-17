module System.Internal.Component exposing (wrapEvents, wrapInit, wrapLayoutView, wrapSubscriptions, wrapToTuple, wrapUiView, wrapUpdate)

import Json.Decode as Decode
import System.Event exposing (ComponentEventHandlers)
import System.Internal.Event exposing (Event(..), EventHandler, mapEventHandler)
import System.Internal.Message exposing (Control(..), SystemMessage(..))
import System.Internal.PID exposing (PID)
import System.Message exposing (sendToPid)


type alias Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut =
    { a
        | wrapModel : componentModel -> appModel
        , wrapMsg : componentMsgIn -> appMsg
        , mapIn : appMsg -> Maybe componentMsgIn
        , mapOut : PID -> componentMsgOut -> SystemMessage addresses actors appMsg
    }


wrapInit :
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    ->
        (( PID, Decode.Value )
         -> ( componentModel, List componentMsgOut, Cmd componentMsgIn )
        )
    -> ( PID, Decode.Value )
    -> ( appModel, SystemMessage addresses actors appMsg )
wrapInit args init ( pid, flags ) =
    init ( pid, flags )
        |> wrapToTuple args pid


wrapUpdate :
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    ->
        (componentMsgIn
         -> componentModel
         -> ( componentModel, List componentMsgOut, Cmd componentMsgIn )
        )
    -> componentModel
    -> SystemMessage addresses actors appMsg
    -> PID
    -> ( appModel, SystemMessage addresses actors appMsg )
wrapUpdate args update appModel msg pid =
    case msg of
        ActorMsg appMsg ->
            case args.mapIn appMsg of
                Just componentMsgIn ->
                    update componentMsgIn appModel
                        |> wrapToTuple args pid

                Nothing ->
                    ( args.wrapModel appModel, UnmappedMsg appMsg )

        _ ->
            ( args.wrapModel appModel, NoOp )


wrapSubscriptions :
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    -> (componentModel -> Sub componentMsgIn)
    -> componentModel
    -> PID
    -> Sub (SystemMessage addresses actors appMsg)
wrapSubscriptions { wrapMsg } subs appModel pid =
    if subs appModel == Sub.none then
        Sub.none

    else
        Sub.map (sendToPid pid << wrapMsg) (subs appModel)


wrapEvents :
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    -> ComponentEventHandlers componentMsgIn
    -> Event
    -> PID
    -> EventHandler (SystemMessage addresses actors appMsg)
wrapEvents { wrapMsg } { onPIDNotFound, onKill } event pid =
    case event of
        OnPIDNotFound pidNotFound ->
            mapEventHandler (sendToPid pid << wrapMsg) (onPIDNotFound pidNotFound)

        OnKill ->
            mapEventHandler (sendToPid pid << wrapMsg) onKill


wrapToTuple :
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    -> PID
    -> ( componentModel, List componentMsgOut, Cmd componentMsgIn )
    -> ( appModel, SystemMessage addresses actors appMsg )
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
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    ->
        ((componentMsgIn -> SystemMessage addresses actors appMsg)
         -> componentModel
         -> (PID -> Maybe output)
         -> output
        )
    -> componentModel
    -> PID
    -> (PID -> Maybe output)
    -> output
wrapLayoutView { wrapMsg } view model pid renderPid =
    view
        (sendToPid pid << wrapMsg)
        model
        renderPid


wrapUiView :
    Args a addresses actors appModel appMsg componentModel componentMsgIn componentMsgOut
    ->
        ((componentMsgIn -> SystemMessage addresses actors appMsg)
         -> componentModel
         -> output
        )
    -> componentModel
    -> PID
    -> (PID -> Maybe output)
    -> output
wrapUiView { wrapMsg } view model pid _ =
    view
        (sendToPid pid << wrapMsg)
        model
