module System.Internal.Browser exposing (ApplicationArguments, ElementArguments, application, element, toProgramApplicationRecord, toProgramElementRecord)

import Browser as Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Json.Decode as Decode
import System.Internal.Message exposing (LogMessage, SystemMessage)
import System.Internal.Model as SystemModel exposing (SystemModel)
import System.Internal.PID exposing (PID)
import System.Internal.Render as Render
import System.Internal.SystemActor exposing (SystemActor(..))
import System.Internal.Update exposing (update)
import System.Message exposing (batch)
import System.Platform exposing (Program)
import Url exposing (Url)


type alias ElementArguments flags applicationAddress applicationActorName componentModel componentOutput applicationMessage =
    { apply :
        componentModel
        -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    , factory :
        applicationActorName
        -> ( PID, Decode.Value )
        -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
    , init :
        flags
        -> List (SystemMessage applicationAddress applicationActorName applicationMessage)
    , view :
        List componentOutput
        -> Html.Html (SystemMessage applicationAddress applicationActorName applicationMessage)
    , onLogMessage :
        LogMessage applicationAddress applicationActorName applicationMessage
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    }


type alias ApplicationArguments flags applicationAddress applicationActorName componentModel componentOutput applicationMessage =
    { apply :
        componentModel
        -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    , factory :
        applicationActorName
        -> ( PID, Decode.Value )
        -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
    , init :
        flags
        -> Url
        -> Key
        -> List (SystemMessage applicationAddress applicationActorName applicationMessage)
    , view :
        List componentOutput
        -> List (Html (SystemMessage applicationAddress applicationActorName applicationMessage))
    , onUrlRequest :
        Browser.UrlRequest
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    , onUrlChange :
        Url
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    , onLogMessage :
        LogMessage applicationAddress applicationActorName applicationMessage
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    }


element :
    ElementArguments flags applicationAddress applicationActorName componentModel componentOutput applicationMessage
    -> Program flags applicationAddress applicationActorName componentModel applicationMessage
element =
    Browser.element << toProgramElementRecord


application :
    ApplicationArguments flags applicationAddress applicationActorName componentModel componentOutput applicationMessage
    -> Program flags applicationAddress applicationActorName componentModel applicationMessage
application =
    Browser.application << toProgramApplicationRecord


toProgramElementRecord :
    ElementArguments flags applicationAddress applicationActorName componentModel componentOutput applicationMessage
    ->
        { init :
            flags
            -> ( SystemModel applicationAddress applicationActorName componentModel, Cmd (SystemMessage applicationAddress applicationActorName applicationMessage) )
        , subscriptions :
            SystemModel applicationAddress applicationActorName componentModel
            -> Sub (SystemMessage applicationAddress applicationActorName applicationMessage)
        , update :
            SystemMessage applicationAddress applicationActorName applicationMessage
            -> SystemModel applicationAddress applicationActorName componentModel
            -> ( SystemModel applicationAddress applicationActorName componentModel, Cmd (SystemMessage applicationAddress applicationActorName applicationMessage) )
        , view :
            SystemModel applicationAddress applicationActorName componentModel
            -> Html (SystemMessage applicationAddress applicationActorName applicationMessage)
        }
toProgramElementRecord implementation =
    { init =
        \flags ->
            update implementation Nothing (batch (implementation.init flags)) SystemModel.init
    , update = update implementation Nothing
    , subscriptions =
        Sub.batch
            << SystemModel.foldlInstances
                (\{ pid, componentModel } subs ->
                    let
                        (SystemActor systemActor) =
                            implementation.apply componentModel
                    in
                    if systemActor.subscriptions pid == Sub.none then
                        subs

                    else
                        systemActor.subscriptions pid :: subs
                )
                []
    , view = implementation.view << Render.view implementation
    }


toProgramApplicationRecord :
    ApplicationArguments flags applicationAddress applicationActorName componentModel componentOutput applicationMessage
    ->
        { init :
            flags
            -> Url
            -> Key
            -> ( SystemModel applicationAddress applicationActorName componentModel, Cmd (SystemMessage applicationAddress applicationActorName applicationMessage) )
        , onUrlChange : Url -> SystemMessage applicationAddress applicationActorName applicationMessage
        , onUrlRequest : UrlRequest -> SystemMessage applicationAddress applicationActorName applicationMessage
        , subscriptions :
            SystemModel applicationAddress1 applicationActorName1 componentModel
            -> Sub (SystemMessage applicationAddress applicationActorName applicationMessage)
        , update :
            SystemMessage applicationAddress applicationActorName applicationMessage
            -> SystemModel applicationAddress applicationActorName componentModel
            -> ( SystemModel applicationAddress applicationActorName componentModel, Cmd (SystemMessage applicationAddress applicationActorName applicationMessage) )
        , view :
            SystemModel applicationAddress applicationActorName componentModel
            -> Document (SystemMessage applicationAddress applicationActorName applicationMessage)
        }
toProgramApplicationRecord implementation =
    { init =
        \flags url key ->
            update implementation Nothing (batch (implementation.init flags url key)) SystemModel.init
    , update = update implementation Nothing
    , subscriptions =
        Sub.batch
            << SystemModel.foldlInstances
                (\{ pid, componentModel } subs ->
                    let
                        (SystemActor systemActor) =
                            implementation.apply componentModel
                    in
                    if systemActor.subscriptions pid == Sub.none then
                        subs

                    else
                        systemActor.subscriptions pid :: subs
                )
                []
    , view = Render.viewApplication implementation
    , onUrlRequest = implementation.onUrlRequest
    , onUrlChange = implementation.onUrlChange
    }
