module System.Browser exposing
    ( application
    , element
    , applicationRecord, elementRecord
    )

{-| This module helps you set up an System Program.


## Type Prefixes

when a type variable is prefixed with

  - `component*` Your component should provide this type
  - `application*` Your application should provide this type
  - `system*` The system will provide this type


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
        componentModel
        -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    , factory :
        applicationActorName
        -> ( PID, Value )
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
        UrlRequest
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    , onUrlChange :
        Url
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    , onLogMessage :
        LogMessage applicationAddress applicationActorName applicationMessage
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    }
    -> Program flags applicationAddress applicationActorName componentModel applicationMessage
application =
    SystemBrowser.application


{-| Create an HTML element managed by Elm through a System.
-}
element :
    { apply :
        componentModel
        -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    , factory :
        applicationActorName
        -> ( PID, Value )
        -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
    , init :
        flags
        -> List (SystemMessage applicationAddress applicationActorName applicationMessage)
    , view :
        List componentOutput
        -> Html (SystemMessage applicationAddress applicationActorName applicationMessage)
    , onLogMessage :
        LogMessage applicationAddress applicationActorName applicationMessage
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    }
    -> Program flags applicationAddress applicationActorName componentModel applicationMessage
element =
    SystemBrowser.element


{-| Returns the record that is used by Browser.element
-}
elementRecord :
    { apply :
        componentModel
        -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    , factory :
        applicationActorName
        -> ( PID, Value )
        -> ( componentModel, SystemMessage applicationAddress applicationActorName applicationMessage )
    , init :
        flags
        -> List (SystemMessage applicationAddress applicationActorName applicationMessage)
    , view :
        List componentOutput
        -> Html (SystemMessage applicationAddress applicationActorName applicationMessage)
    , onLogMessage :
        LogMessage applicationAddress applicationActorName applicationMessage
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    }
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
elementRecord =
    SystemBrowser.toProgramElementRecord


{-| Returns the record that is used by Browser.application
-}
applicationRecord :
    { apply :
        componentModel
        -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    , factory :
        applicationActorName
        -> ( PID, Value )
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
        UrlRequest
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    , onUrlChange :
        Url
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    , onLogMessage :
        LogMessage applicationAddress applicationActorName applicationMessage
        -> SystemMessage applicationAddress applicationActorName applicationMessage
    }
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
applicationRecord =
    SystemBrowser.toProgramApplicationRecord
