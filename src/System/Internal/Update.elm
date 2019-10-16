module System.Internal.Update exposing (update)

import Json.Encode as Encode
import System.Internal.Event exposing (Event(..), EventHandler(..))
import System.Internal.Message exposing (Control(..), LogMessage(..), Message(..), Severity(..), toString)
import System.Internal.Model exposing (Model, addAddress, addView, getAddress, getChildren, getInstance, getNewPID, removePID, updateDocumentTitle, updateInstance)
import System.Internal.PID exposing (PID, system)
import System.Internal.SystemActor exposing (SystemActor(..))


update :
    { a
        | apply :
            appModel
            -> SystemActor appModel output (Message address actorName appMsg)
        , factory :
            actorName
            -> ( PID, Encode.Value )
            -> ( appModel, Message address actorName appMsg )
        , onLogMessage :
            LogMessage address actorName appMsg
            -> Message address actorName appMsg
    }
    -> Maybe PID
    -> Message address actorName appMsg
    -> Model address actorName appModel
    -> ( Model address actorName appModel, Cmd (Message address actorName appMsg) )
update impl maybePid msg model =
    case msg of
        Control (Batch []) ->
            ( model, Cmd.none )

        Control (Batch listOfMsgs) ->
            List.foldl
                (\batchMsg previous ->
                    cmdAndThen (update impl maybePid batchMsg) previous
                )
                ( model, Cmd.none )
                listOfMsgs

        Control (Command cmd) ->
            ( model, cmd )

        Control (SendToPID pid sendToPidMsg) ->
            case getInstance pid model of
                Just ( actorName, appModel ) ->
                    let
                        appMsgs =
                            collectappMsgs sendToPidMsg

                        ( updatedModel, newMsg ) =
                            List.foldl
                                (\appMsg ( model_, msg_ ) ->
                                    let
                                        (SystemActor appliedActor) =
                                            impl.apply model_

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
                                    (\actorModel -> updateInstance pid actorName actorModel model)
                    in
                    update impl maybePid newMsg updatedModel

                Nothing ->
                    maybePid
                        |> Maybe.map
                            (\senderPID ->
                                case getInstance senderPID model of
                                    Just ( _, actorModel ) ->
                                        let
                                            (SystemActor appliedActor) =
                                                impl.apply actorModel
                                        in
                                        case appliedActor.events (OnPIDNotFound pid) senderPID of
                                            Default ->
                                                ( model, Cmd.none )

                                            Ignore ->
                                                ( model, Cmd.none )

                                            BeforeDefault senderMsg ->
                                                update impl maybePid senderMsg model

                                            Custom senderMsg ->
                                                update impl maybePid senderMsg model

                                    Nothing ->
                                        ( model, Cmd.none )
                            )
                        |> Maybe.withDefault
                            ( model, Cmd.none )

        Control (SendToAddress address sendToAddressMsg) ->
            update impl
                maybePid
                (Control
                    (Batch
                        (getAddress address model
                            |> Tuple.second
                            |> List.map
                                (\pid -> Control (SendToPID pid sendToAddressMsg))
                        )
                    )
                )
                model

        Control (Spawn actorName replyMsg) ->
            let
                ( newPID, updateModel ) =
                    getNewPID maybePid model

                ( m3, newMsg ) =
                    impl.factory actorName ( newPID, Encode.null )
                        |> Tuple.mapFirst
                            (\actorModel ->
                                updateInstance newPID actorName actorModel updateModel
                            )
            in
            update impl maybePid newMsg m3
                |> cmdAndThen (update impl maybePid (replyMsg newPID))

        Control (SpawnWithFlags flags actorName replyMsg) ->
            let
                ( newPID, updateModel ) =
                    getNewPID maybePid model

                ( m3, newMsg ) =
                    impl.factory actorName ( newPID, flags )
                        |> Tuple.mapFirst
                            (\actorModel ->
                                updateInstance newPID actorName actorModel updateModel
                            )
            in
            update impl maybePid newMsg m3
                |> cmdAndThen (update impl maybePid (replyMsg newPID))

        Control (AddView pid) ->
            ( addView pid model
            , Cmd.none
            )

        Control (PopulateAddress address pid) ->
            ( addAddress address pid model
            , Cmd.none
            )

        Control (Kill pid) ->
            handleKill impl maybePid pid model

        Context pid systemMsg ->
            update impl (Just pid) systemMsg model

        UpdateDocumentTitle documentTitle ->
            ( updateDocumentTitle documentTitle model
            , Cmd.none
            )

        Log logMessage ->
            update impl maybePid (impl.onLogMessage logMessage) model

        NoOp ->
            ( model, Cmd.none )

        ActorMsg _ ->
            ( model, Cmd.none )

        UnmappedMsg _ ->
            ( model, Cmd.none )


handleKill :
    { a
        | apply :
            appModel
            -> SystemActor appModel output (Message address actorName appMsg)
        , factory :
            actorName
            -> ( PID, Encode.Value )
            -> ( appModel, Message address actorName appMsg )
        , onLogMessage :
            LogMessage address actorName appMsg
            -> Message address actorName appMsg
    }
    -> Maybe PID
    -> PID
    -> Model address actorName appModel
    -> ( Model address actorName appModel, Cmd (Message address actorName appMsg) )
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
                            (\childPID previous ->
                                cmdAndThen (handleKill impl maybePid childPID) previous
                            )
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
    Message address actorName appMsg
    -> List (Message address actorName appMsg)
collectappMsgs msg =
    case msg of
        ActorMsg _ ->
            [ msg ]

        Control (Batch list) ->
            List.concatMap collectappMsgs list

        _ ->
            []


composeSysMsg :
    Message address actorName appMsg
    -> Message address actorName appMsg
    -> Message address actorName appMsg
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
