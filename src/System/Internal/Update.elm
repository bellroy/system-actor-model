module System.Internal.Update exposing (update)

import Dict
import Set
import System.Internal.Event exposing (Event(..), EventHandler(..))
import System.Internal.Message exposing (Control(..), Message(..))
import System.Internal.Model exposing (Model, addAddress, addView, getAddress, getInstance, getNewPID, removePID, updateDocumentTitle, updateInstance)
import System.Internal.PID exposing (PID, toInt)
import System.Internal.SystemActor exposing (SystemActor(..))


update :
    { a
        | apply : appModel -> SystemActor appModel output (Message address actorName appMsg)
        , factory : actorName -> PID -> ( appModel, Message address actorName appMsg )
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
                            collectwrappedMsgs sendToPidMsg

                        ( updatedModel, newMsg ) =
                            List.foldl
                                (\appMsg ( model_, msg_ ) ->
                                    let
                                        (SystemActor appliedActor) =
                                            impl.apply model_

                                        ( actorModelUpdated, wrappedMsg_ ) =
                                            appliedActor.update appMsg pid
                                    in
                                    ( actorModelUpdated
                                    , composeSysMsg msg_ wrappedMsg_
                                    )
                                )
                                ( appModel, NoOp )
                                appMsgs
                                |> Tuple.mapFirst (updateInstance model pid actorName)
                    in
                    update impl maybePid newMsg updatedModel

                Nothing ->
                    maybePid
                        |> Maybe.map
                            (\senderPID ->
                                case getInstance senderPID model of
                                    Just ( actorName, actorModel ) ->
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
                    impl.factory actorName newPID
                        |> Tuple.mapFirst (updateInstance updateModel newPID actorName)
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

        NoOp ->
            ( model, Cmd.none )

        ActorMsg _ ->
            ( model, Cmd.none )

        UnmappedMsg appMsg ->
            ( model, Cmd.none )


handleKill :
    { a
        | apply : appModel -> SystemActor appModel output (Message address actorName appMsg)
        , factory : actorName -> PID -> ( appModel, Message address actorName appMsg )
    }
    -> Maybe PID
    -> PID
    -> Model address actorName appModel
    -> ( Model address actorName appModel, Cmd (Message address actorName appMsg) )
handleKill impl maybePid pid model =
    let
        -- @TODO This requires work
        children =
            []

        -- Dict.get (PID.toInt pid) model.children
        --     |> Maybe.map
        --         (Set.toList
        --             >> List.filterMap
        --                 (\key ->
        --                     Dict.get key model.instances
        --                         |> Maybe.map .pid
        --                 )
        --         )
        --     |> Maybe.withDefault []
    in
    case getInstance pid model of
        Just ( actorName, appModel ) ->
            let
                (SystemActor applied) =
                    impl.apply appModel

                event =
                    applied.events OnKill pid
            in
            case event of
                Default ->
                    children
                        |> List.foldl
                            (\childPID previous ->
                                cmdAndThen (handleKill impl maybePid childPID) previous
                            )
                            ( model, Cmd.none )
                        |> Tuple.mapFirst (removePID pid)

                Ignore ->
                    ( model, Cmd.none )

                BeforeDefault msgIn ->
                    let
                        ( updatedModel, cmd ) =
                            update impl maybePid msgIn model
                    in
                    ( removePID pid updatedModel
                    , cmd
                    )

                Custom msgIn ->
                    update impl maybePid msgIn model

        Nothing ->
            ( model, Cmd.none )


collectwrappedMsgs :
    Message address actorName appMsg
    -> List (Message address actorName appMsg)
collectwrappedMsgs msg =
    case msg of
        ActorMsg _ ->
            [ msg ]

        Control (Batch list) ->
            List.concatMap collectwrappedMsgs list

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
