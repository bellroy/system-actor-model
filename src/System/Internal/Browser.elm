module System.Internal.Browser exposing (ApplicationArguments, ElementArguments, application, element, toProgramApplicationRecord, toProgramElementRecord)

import Browser as Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Json.Decode exposing (Value)
import System.Internal.Message exposing (LogMessage, SystemMessage)
import System.Internal.Model as SystemModel exposing (SystemModel)
import System.Internal.PID exposing (PID)
import System.Internal.Render as Render
import System.Internal.SystemActor exposing (SystemActor(..))
import System.Internal.Update exposing (update)
import System.Message exposing (batch)
import System.Platform exposing (Program)
import Url exposing (Url)


type alias ElementArguments flags addresses actors appModel output appMsg =
    { apply :
        appModel
        -> SystemActor appModel output (SystemMessage addresses actors appMsg)
    , factory :
        actors
        -> ( PID, Value )
        -> ( appModel, SystemMessage addresses actors appMsg )
    , init :
        flags
        -> List (SystemMessage addresses actors appMsg)
    , view :
        List output
        -> Html.Html (SystemMessage addresses actors appMsg)
    , onLogMessage :
        LogMessage addresses actors appMsg
        -> SystemMessage addresses actors appMsg
    }


type alias ApplicationArguments flags addresses actors appModel output appMsg =
    { apply :
        appModel
        -> SystemActor appModel output (SystemMessage addresses actors appMsg)
    , factory :
        actors
        -> ( PID, Value )
        -> ( appModel, SystemMessage addresses actors appMsg )
    , init :
        flags
        -> Url
        -> Key
        -> List (SystemMessage addresses actors appMsg)
    , view :
        List output
        -> List (Html (SystemMessage addresses actors appMsg))
    , onUrlRequest :
        Browser.UrlRequest
        -> SystemMessage addresses actors appMsg
    , onUrlChange :
        Url
        -> SystemMessage addresses actors appMsg
    , onLogMessage :
        LogMessage addresses actors appMsg
        -> SystemMessage addresses actors appMsg
    }


element :
    ElementArguments flags addresses actors appModel output appMsg
    -> Program flags addresses actors appModel appMsg
element =
    Browser.element << toProgramElementRecord


application :
    ApplicationArguments flags addresses actors appModel output appMsg
    -> Program flags addresses actors appModel appMsg
application =
    Browser.application << toProgramApplicationRecord


toProgramElementRecord :
    ElementArguments flags addresses actors appModel output appMsg
    ->
        { init :
            flags
            -> ( SystemModel addresses actors appModel, Cmd (SystemMessage addresses actors appMsg) )
        , subscriptions :
            SystemModel addresses actors appModel
            -> Sub (SystemMessage addresses actors appMsg)
        , update :
            SystemMessage addresses actors appMsg
            -> SystemModel addresses actors appModel
            -> ( SystemModel addresses actors appModel, Cmd (SystemMessage addresses actors appMsg) )
        , view :
            SystemModel addresses actors appModel
            -> Html (SystemMessage addresses actors appMsg)
        }
toProgramElementRecord implementation =
    { init =
        \flags ->
            update implementation Nothing (batch (implementation.init flags)) SystemModel.init
    , update = update implementation Nothing
    , subscriptions =
        Sub.batch
            << SystemModel.foldlInstances
                (\{ pid, appModel } subs ->
                    let
                        (SystemActor systemActor) =
                            implementation.apply appModel
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
    ApplicationArguments flags addresses actors appModel output appMsg
    ->
        { init :
            flags
            -> Url
            -> Key
            -> ( SystemModel addresses actors appModel, Cmd (SystemMessage addresses actors appMsg) )
        , onUrlChange : Url -> SystemMessage addresses actors appMsg
        , onUrlRequest : UrlRequest -> SystemMessage addresses actors appMsg
        , subscriptions :
            SystemModel addresses1 actors1 appModel
            -> Sub (SystemMessage addresses actors appMsg)
        , update :
            SystemMessage addresses actors appMsg
            -> SystemModel addresses actors appModel
            -> ( SystemModel addresses actors appModel, Cmd (SystemMessage addresses actors appMsg) )
        , view :
            SystemModel addresses actors appModel
            -> Document (SystemMessage addresses actors appMsg)
        }
toProgramApplicationRecord implementation =
    { init =
        \flags url key ->
            update implementation Nothing (batch (implementation.init flags url key)) SystemModel.init
    , update = update implementation Nothing
    , subscriptions =
        Sub.batch
            << SystemModel.foldlInstances
                (\{ pid, appModel } subs ->
                    let
                        (SystemActor systemActor) =
                            implementation.apply appModel
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
