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
            appModel
            -> SystemActor appModel output (SystemMessage addresses actors appMsg)
        , factory :
            actors
            -> ( PID, Value )
            -> ( appModel, SystemMessage addresses actors appMsg )
        , onLogMessage :
            LogMessage addresses actors appMsg
            -> SystemMessage addresses actors appMsg
    }
    -> Maybe PID
    -> SystemMessage addresses actors appMsg
    -> SystemModel addresses actors appModel
    -> ( SystemModel addresses actors appModel, Cmd (SystemMessage addresses actors appMsg) )
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
                Just ( actors, appModel ) ->
                    let
                        appMsgs =
                            collectappMsgs sendToPidMsg

                        ( updatedModel, newMsg ) =
                            List.foldl
                                (\appMsg ( systemModel_, msg_ ) ->
                                    let
                                        (SystemActor appliedActor) =
                                            impl.apply systemModel_

                                        ( actorModelUpdated, appMsg_ ) =
                                            appliedActor.update appMsg pid
                                    in
                                    ( actorModelUpdated
                                    , composeSysMsg msg_ appMsg_
                                    )
                                )
                                ( appModel, NoOp )
                                appMsgs
                                |> Tuple.mapFirst
                                    (updateInstance pid actors systemModel)
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

        Control (SendToAddress addresses sendToAddressMsg) ->
            update impl
                maybePid
                (Control
                    (Batch
                        (getAddress addresses systemModel
                            |> Tuple.second
                            |> List.map
                                (\pid -> Control (SendToPID pid sendToAddressMsg))
                        )
                    )
                )
                systemModel

        Control (SendToPidOnAddress pid addresses sendToPidOnAddressMsg) ->
            let
                pidIsOnAddress =
                    getAddress addresses systemModel
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

        Control (SpawnWithFlags flags actors replyMsg) ->
            let
                ( newPID, m2 ) =
                    getNewPID maybePid systemModel

                ( m3, newMsg ) =
                    impl.factory actors ( newPID, flags )
                        |> Tuple.mapFirst
                            (updateInstance newPID actors m2)
            in
            update impl maybePid newMsg m3
                |> cmdAndThen (update impl maybePid (replyMsg newPID))

        Control (SpawnMultipleWithFlags listActorNamesAndFlags replyMsg) ->
            let
                ( updatedModel, cmds, pids ) =
                    listActorNamesAndFlags
                        |> List.foldl
                            (\( actorName, flags ) ( m_, cmds_, pids_ ) ->
                                let
                                    ( newPID, m2_ ) =
                                        getNewPID maybePid m_

                                    ( m3_, newMsg ) =
                                        impl.factory actorName ( newPID, flags )
                                            |> Tuple.mapFirst
                                                (updateInstance newPID actorName m2_)

                                    ( m4_, cmd ) =
                                        update impl maybePid newMsg m3_
                                in
                                ( m4_
                                , Cmd.batch [ cmds_, cmd ]
                                , List.append pids_ [ newPID ]
                                )
                            )
                            ( systemModel, Cmd.none, [] )
            in
            cmdAndThen (update impl maybePid (replyMsg pids))
                ( updatedModel, cmds )

        Control (AddView pid) ->
            ( addView pid systemModel
            , Cmd.none
            )

        Control (PopulateAddress addresses pid) ->
            ( addAddress addresses pid systemModel
            , Cmd.none
            )

        Control (RemoveFromView pid) ->
            ( removeFromView pid systemModel
            , Cmd.none
            )

        Control (RemoveFromAddress addresses pid) ->
            ( removeFromAddress addresses pid systemModel
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
            appModel
            -> SystemActor appModel output (SystemMessage addresses actors appMsg)
        , factory :
            actors
            -> ( PID, Value )
            -> ( appModel, SystemMessage addresses actors appMsg )
        , onLogMessage :
            LogMessage addresses actors appMsg
            -> SystemMessage addresses actors appMsg
    }
    -> Maybe PID
    -> PID
    -> SystemModel addresses actors appModel
    -> ( SystemModel addresses actors appModel, Cmd (SystemMessage addresses actors appMsg) )
handleKill impl maybePid pid model =
    case getInstance pid model of
        Just ( _, appModel ) ->
            let
                (SystemActor applied) =
                    impl.apply appModel

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


collectappMsgs :
    SystemMessage addresses actors appMsg
    -> List (SystemMessage addresses actors appMsg)
collectappMsgs msg =
    case msg of
        ActorMsg _ ->
            [ msg ]

        Control (Batch list) ->
            List.concatMap collectappMsgs list

        _ ->
            []


composeSysMsg :
    SystemMessage addresses actors appMsg
    -> SystemMessage addresses actors appMsg
    -> SystemMessage addresses actors appMsg
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
