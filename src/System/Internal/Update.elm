module System.Internal.Update exposing (update)

import Json.Encode as Encode exposing (Value)
import System.Internal.Event exposing (Event(..), EventHandler(..))
import System.Internal.Message exposing (Control(..), LogMessage(..), Severity(..), SystemMessage(..))
import System.Internal.Model
    exposing
        ( SystemModel
        , addAddress
        , addView
        , getAddress
        , getChildren
        , getInstance
        , getNewPID
        , removeFromAddress
        , removeFromView
        , removePID
        , updateDocumentTitle
        , updateInstance
        )
import System.Internal.PID exposing (PID)
import System.Internal.SystemActor exposing (SystemActor(..))


update :
    { a
        | apply :
            applicationModel
            -> SystemActor applicationModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
        , factory :
            applicationActorName
            -> ( PID, Value )
            -> ( applicationModel, SystemMessage applicationAddress applicationActorName applicationMessage )
        , onLogMessage :
            LogMessage applicationAddress applicationActorName applicationMessage
            -> SystemMessage applicationAddress applicationActorName applicationMessage
    }
    -> Maybe PID
    -> SystemMessage applicationAddress applicationActorName applicationMessage
    -> SystemModel applicationAddress applicationActorName applicationModel
    -> ( SystemModel applicationAddress applicationActorName applicationModel, Cmd (SystemMessage applicationAddress applicationActorName applicationMessage) )
update impl maybePid msg systemModel =
    case msg of
        Control (Batch []) ->
            ( systemModel, Cmd.none )

        Control (Batch listOfMsgs) ->
            List.foldl
                (cmdAndThen << update impl maybePid)
                ( systemModel, Cmd.none )
                listOfMsgs

        Control (Command cmd) ->
            ( systemModel, cmd )

        Control (SendToPID pid sendToPidMsg) ->
            case getInstance pid systemModel of
                Just ( applicationActorName, applicationModel ) ->
                    let
                        applicationMessages =
                            collectapplicationMessages sendToPidMsg

                        ( updatedModel, newMsg ) =
                            List.foldl
                                (\applicationMessage ( systemModel_, msg_ ) ->
                                    let
                                        (SystemActor appliedActor) =
                                            impl.apply systemModel_

                                        ( actorModelUpdated, applicationMessage_ ) =
                                            appliedActor.update applicationMessage pid
                                    in
                                    ( actorModelUpdated
                                    , composeSysMsg msg_ applicationMessage_
                                    )
                                )
                                ( applicationModel, NoOp )
                                applicationMessages
                                |> Tuple.mapFirst
                                    (updateInstance pid applicationActorName systemModel)
                    in
                    update impl maybePid newMsg updatedModel

                Nothing ->
                    maybePid
                        |> Maybe.map
                            (\senderPID ->
                                case getInstance senderPID systemModel of
                                    Just ( _, actorModel ) ->
                                        let
                                            (SystemActor appliedActor) =
                                                impl.apply actorModel
                                        in
                                        case appliedActor.events (OnPIDNotFound pid) senderPID of
                                            Default ->
                                                ( systemModel, Cmd.none )

                                            Ignore ->
                                                ( systemModel, Cmd.none )

                                            BeforeDefault senderMsg ->
                                                update impl maybePid senderMsg systemModel

                                            Custom senderMsg ->
                                                update impl maybePid senderMsg systemModel

                                    Nothing ->
                                        ( systemModel, Cmd.none )
                            )
                        |> Maybe.withDefault
                            ( systemModel, Cmd.none )

        Control (SendToAddress applicationAddress sendToAddressMsg) ->
            update impl
                maybePid
                (Control
                    (Batch
                        (getAddress applicationAddress systemModel
                            |> Tuple.second
                            |> List.map
                                (\pid -> Control (SendToPID pid sendToAddressMsg))
                        )
                    )
                )
                systemModel

        Control (SendToPidOnAddress pid applicationAddress sendToPidOnAddressMsg) ->
            let
                pidIsOnAddress =
                    getAddress applicationAddress systemModel
                        |> Tuple.second
                        |> List.member pid
            in
            if pidIsOnAddress then
                update impl
                    maybePid
                    (Control (SendToPID pid sendToPidOnAddressMsg))
                    systemModel

            else
                ( systemModel, Cmd.none )

        Control (Spawn applicationActorName replyMsg) ->
            let
                ( newPID, updateModel ) =
                    getNewPID maybePid systemModel

                ( m3, newMsg ) =
                    impl.factory applicationActorName ( newPID, Encode.null )
                        |> Tuple.mapFirst
                            (updateInstance newPID applicationActorName updateModel)
            in
            update impl maybePid newMsg m3
                |> cmdAndThen (update impl maybePid (replyMsg newPID))

        Control (SpawnWithFlags flags applicationActorName replyMsg) ->
            let
                ( newPID, updateModel ) =
                    getNewPID maybePid systemModel

                ( m3, newMsg ) =
                    impl.factory applicationActorName ( newPID, flags )
                        |> Tuple.mapFirst
                            (updateInstance newPID applicationActorName updateModel)
            in
            update impl maybePid newMsg m3
                |> cmdAndThen (update impl maybePid (replyMsg newPID))

        Control (AddView pid) ->
            ( addView pid systemModel
            , Cmd.none
            )

        Control (PopulateAddress applicationAddress pid) ->
            ( addAddress applicationAddress pid systemModel
            , Cmd.none
            )

        Control (RemoveFromView pid) ->
            ( removeFromView pid systemModel
            , Cmd.none
            )

        Control (RemoveFromAddress applicationAddress pid) ->
            ( removeFromAddress applicationAddress pid systemModel
            , Cmd.none
            )

        Control (Kill pid) ->
            handleKill impl maybePid pid systemModel

        Context pid systemMsg ->
            update impl (Just pid) systemMsg systemModel

        UpdateDocumentTitle documentTitle ->
            ( updateDocumentTitle documentTitle systemModel
            , Cmd.none
            )

        Log logMessage ->
            update impl maybePid (impl.onLogMessage logMessage) systemModel

        NoOp ->
            ( systemModel, Cmd.none )

        ActorMsg _ ->
            ( systemModel, Cmd.none )

        UnmappedMsg _ ->
            ( systemModel, Cmd.none )


handleKill :
    { a
        | apply :
            applicationModel
            -> SystemActor applicationModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
        , factory :
            applicationActorName
            -> ( PID, Value )
            -> ( applicationModel, SystemMessage applicationAddress applicationActorName applicationMessage )
        , onLogMessage :
            LogMessage applicationAddress applicationActorName applicationMessage
            -> SystemMessage applicationAddress applicationActorName applicationMessage
    }
    -> Maybe PID
    -> PID
    -> SystemModel applicationAddress applicationActorName applicationModel
    -> ( SystemModel applicationAddress applicationActorName applicationModel, Cmd (SystemMessage applicationAddress applicationActorName applicationMessage) )
handleKill impl maybePid pid model =
    case getInstance pid model of
        Just ( _, applicationModel ) ->
            let
                (SystemActor applied) =
                    impl.apply applicationModel

                event =
                    applied.events OnKill pid
            in
            case event of
                Ignore ->
                    ( model, Cmd.none )

                Custom msgIn ->
                    update impl maybePid msgIn model

                Default ->
                    getChildren pid model
                        |> Maybe.withDefault []
                        |> List.foldl
                            (handleKill impl maybePid >> cmdAndThen)
                            ( model, Cmd.none )
                        |> Tuple.mapFirst (removePID pid)

                BeforeDefault msgIn ->
                    let
                        ( updatedModel, cmd ) =
                            update impl maybePid msgIn model
                    in
                    ( removePID pid updatedModel
                    , cmd
                    )

        Nothing ->
            ( model, Cmd.none )


collectapplicationMessages :
    SystemMessage applicationAddress applicationActorName applicationMessage
    -> List (SystemMessage applicationAddress applicationActorName applicationMessage)
collectapplicationMessages msg =
    case msg of
        ActorMsg _ ->
            [ msg ]

        Control (Batch list) ->
            List.concatMap collectapplicationMessages list

        _ ->
            []


composeSysMsg :
    SystemMessage applicationAddress applicationActorName applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
    -> SystemMessage applicationAddress applicationActorName applicationMessage
composeSysMsg a b =
    if a == NoOp then
        b

    else if b == NoOp then
        a

    else
        Control (Batch [ a, b ])


cmdAndThen :
    (m -> ( m, Cmd msg ))
    -> ( m, Cmd msg )
    -> ( m, Cmd msg )
cmdAndThen f ( model, cmd ) =
    let
        ( m_, cmd_ ) =
            f model
    in
    ( m_, Cmd.batch [ cmd, cmd_ ] )
