module System.Browser exposing
    ( element
    , application
    )

{-| This module helps you set up an System Program.


# Elements

Create an HTML element managed by Elm.

@docs element


# Applications

Create an application that manages Url changes.

When the application starts, init gets the initial Url. You can show different things depending on the Url!

When someone clicks a link, like <a href="/home">Home</a>, it always goes through onUrlRequest. The resulting message goes to your update function, giving you a chance to save scroll position or persist data before changing the URL yourself with pushUrl or load. More info on this in the UrlRequest docs!

When the URL changes, the new Url goes through onUrlChange. The resulting message goes to update where you can decide what to show next.

Applications always use the Browser.Navigation module for precise control over Url changes.

@docs application

-}

import Browser as Browser exposing (UrlRequest, application, element)
import Browser.Navigation exposing (Key)
import Html exposing (Html)
import System.Internal.Message exposing (Message)
import System.Internal.Model exposing (foldlInstances, init)
import System.Internal.PID exposing (PID)
import System.Internal.Render exposing (view, viewApplication)
import System.Internal.SystemActor exposing (SystemActor(..))
import System.Internal.Update exposing (update)
import System.Message exposing (batch)
import System.Platform exposing (Program)
import Url exposing (Url)


{-| Create an HTML element managed by Elm through a System.
-}
element :
    { apply :
        actorModel
        -> SystemActor actorModel output (Message address actorName wrappedMsg)
    , factory :
        actorName
        -> PID
        -> ( actorModel, Message address actorName wrappedMsg )
    , init :
        flags
        -> List (Message address actorName wrappedMsg)
    , view :
        List output
        -> Html.Html (Message address actorName wrappedMsg)
    }
    -> Program flags address actorName actorModel wrappedMsg
element implementation =
    Browser.element
        { init =
            \flags ->
                update implementation Nothing (batch (implementation.init flags)) init
        , update = update implementation Nothing
        , subscriptions =
            Sub.batch
                << foldlInstances
                    (\{ pid, actorModel } subs ->
                        let
                            (SystemActor systemActor) =
                                implementation.apply actorModel
                        in
                        if systemActor.subscriptions pid == Sub.none then
                            subs

                        else
                            systemActor.subscriptions pid :: subs
                    )
                    []
        , view = implementation.view << view implementation
        }


{-| Create an Application managed by Elm through a System
-}
application :
    { apply :
        actorModel
        -> SystemActor actorModel output (Message address actorName wrappedMsg)
    , factory :
        actorName
        -> PID
        -> ( actorModel, Message address actorName wrappedMsg )
    , init :
        flags
        -> Url
        -> Key
        -> List (Message address actorName wrappedMsg)
    , view :
        List output
        -> List (Html (Message address actorName wrappedMsg))
    , onUrlRequest :
        Browser.UrlRequest
        -> Message address actorName wrappedMsg
    , onUrlChange :
        Url
        -> Message address actorName wrappedMsg
    }
    -> Program flags address actorName actorModel wrappedMsg
application implementation =
    Browser.application
        { init =
            \flags url key ->
                update implementation Nothing (batch (implementation.init flags url key)) init
        , update = update implementation Nothing
        , subscriptions =
            Sub.batch
                << foldlInstances
                    (\{ pid, actorModel } subs ->
                        let
                            (SystemActor systemActor) =
                                implementation.apply actorModel
                        in
                        if systemActor.subscriptions pid == Sub.none then
                            subs

                        else
                            systemActor.subscriptions pid :: subs
                    )
                    []
        , view = viewApplication implementation
        , onUrlRequest = implementation.onUrlRequest
        , onUrlChange = implementation.onUrlChange
        }
