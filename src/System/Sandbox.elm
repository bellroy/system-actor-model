module System.Sandbox exposing
    ( Sandbox, SandboxActor
    , spawn
    , update
    , getView, getModel, getLastOutMsg
    )

{-| This module is under development

@docs Sandbox, SandboxActor

@docs spawn

@docs update

@docs getView, getModel, getLastOutMsg

-}

import Json.Encode as Encode exposing (Value)
import System.Actor exposing (Actor)
import System.Internal.Message exposing (Message(..))
import System.Internal.PID as PID


{-| -}
type alias SandboxActor actorModel actorMsgIn output =
    Actor actorModel actorModel output (Message () () actorMsgIn)


{-| -}
type Sandbox actorModel actorMsgIn output
    = Sandbox
        { actor : SandboxActor actorModel actorMsgIn output
        , model : actorModel
        , outMsgs : List (Message () () actorMsgIn)
        }


{-| -}
spawn :
    SandboxActor actorModel actorMsgIn output
    -> Maybe Value
    -> Sandbox actorModel actorMsgIn output
spawn actor maybeFlags =
    let
        ( model, listOut ) =
            actor.init ( PID.system, maybeFlags |> Maybe.withDefault Encode.null )
    in
    Sandbox
        { actor = actor
        , model = model
        , outMsgs = [ listOut ]
        }


{-| -}
update :
    actorMsgIn
    -> Sandbox actorModel actorMsgIn output
    -> Sandbox actorModel actorMsgIn output
update msgIn (Sandbox sandbox) =
    let
        ( model, listOut ) =
            sandbox.actor.update
                sandbox.model
                (ActorMsg msgIn)
                PID.system
    in
    Sandbox
        { sandbox
            | model = model
            , outMsgs = listOut :: sandbox.outMsgs
        }


{-| -}
getView : Sandbox actorModel actorMsgIn output -> output
getView (Sandbox { actor, model }) =
    actor.view model PID.system (always Nothing)


{-| -}
getModel : Sandbox actorModel actorMsgIn output -> actorModel
getModel (Sandbox { model }) =
    model


{-| -}
getLastOutMsg :
    Sandbox actorModel actorMsgIn output
    -> Maybe (Message () () actorMsgIn)
getLastOutMsg (Sandbox { outMsgs }) =
    List.head outMsgs
