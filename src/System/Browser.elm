module System.Browser exposing
    ( application
    , element
    , applicationRecord, elementRecord
    )

{-| This module helps you set up an System Program.


## Applications

Create an application that manages Url changes.

When the application starts, init gets the initial Url. You can show different things depending on the Url!

When someone clicks a link, like <a href="/home">Home</a>, it always goes through onUrlRequest. The resulting message goes to your update function, giving you a chance to save scroll position or persist data before changing the URL yourself with pushUrl or load. More info on this in the UrlRequest docs!

When the URL changes, the new Url goes through onUrlChange. The resulting message goes to update where you can decide what to show next.

Applications always use the Browser.Navigation module for precise control over Url changes.

@docs application


## Elements

Create an HTML element managed by Elm.

@docs element


## Use alternative programs

Get the records that are used to create the Elm Browser.application and .element

@docs applicationRecord, elementRecord

-}

import Browser exposing (Document, UrlRequest)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import Json.Decode exposing (Value)
import System.Internal.Browser as SystemBrowser
import System.Internal.Message exposing (LogMessage, SystemMessage)
import System.Internal.Model exposing (SystemModel)
import System.Internal.PID exposing (PID)
import System.Internal.SystemActor exposing (SystemActor)
import System.Platform exposing (Program)
import Url exposing (Url)


{-| Create an Application managed by Elm through a System
-}
application :
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
        UrlRequest
        -> SystemMessage addresses actors appMsg
    , onUrlChange :
        Url
        -> SystemMessage addresses actors appMsg
    , onLogMessage :
        LogMessage addresses actors appMsg
        -> SystemMessage addresses actors appMsg
    }
    -> Program flags addresses actors appModel appMsg
application =
    SystemBrowser.application


{-| Create an HTML element managed by Elm through a System.
-}
element :
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
        -> Html (SystemMessage addresses actors appMsg)
    , onLogMessage :
        LogMessage addresses actors appMsg
        -> SystemMessage addresses actors appMsg
    }
    -> Program flags addresses actors appModel appMsg
element =
    SystemBrowser.element


{-| Returns the record that is used by Browser.element
-}
elementRecord :
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
        -> Html (SystemMessage addresses actors appMsg)
    , onLogMessage :
        LogMessage addresses actors appMsg
        -> SystemMessage addresses actors appMsg
    }
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
elementRecord =
    SystemBrowser.toProgramElementRecord


{-| Returns the record that is used by Browser.application
-}
applicationRecord :
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
        UrlRequest
        -> SystemMessage addresses actors appMsg
    , onUrlChange :
        Url
        -> SystemMessage addresses actors appMsg
    , onLogMessage :
        LogMessage addresses actors appMsg
        -> SystemMessage addresses actors appMsg
    }
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
applicationRecord =
    SystemBrowser.toProgramApplicationRecord
