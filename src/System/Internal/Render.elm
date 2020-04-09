module System.Internal.Render exposing
    ( renderPID
    , view
    , viewApplication
    )

import Browser exposing (Document)
import Html exposing (Html)
import System.Internal.Message exposing (SystemMessage(..))
import System.Internal.Model as SystemModel exposing (SystemModel)
import System.Internal.PID exposing (PID)
import System.Internal.SystemActor exposing (SystemActor(..))


view :
    { a
        | apply :
            componentModel
            -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
    }
    -> SystemModel applicationAddress applicationActorName componentModel
    -> List componentOutput
view { apply } systemModel =
    SystemModel.getViews systemModel
        |> List.reverse
        |> List.filterMap
            (renderPID
                (\processModel ->
                    let
                        (SystemActor systemActor) =
                            apply processModel
                    in
                    systemActor.view
                )
                systemModel
            )


viewApplication :
    { a
        | apply :
            componentModel
            -> SystemActor componentModel componentOutput (SystemMessage applicationAddress applicationActorName applicationMessage)
        , view :
            List componentOutput
            -> List (Html (SystemMessage applicationAddress applicationActorName applicationMessage))
    }
    -> SystemModel applicationAddress applicationActorName componentModel
    -> Document (SystemMessage applicationAddress applicationActorName applicationMessage)
viewApplication impl systemModel =
    view impl systemModel
        |> impl.view
        |> (\body ->
                { title = SystemModel.getDocumentTitle systemModel
                , body = body
                }
           )


type alias RenderActor componentModel componentOutput =
    componentModel
    ->
        Maybe
            (PID
             -> (PID -> Maybe componentOutput)
             -> componentOutput
            )


renderPID :
    RenderActor componentModel componentOutput
    -> SystemModel applicationAddress applicationActorName componentModel
    -> PID
    -> Maybe componentOutput
renderPID renderActor systemModel pid =
    SystemModel.getInstance pid systemModel
        |> Maybe.andThen
            (Tuple.second
                >> renderActor
                >> Maybe.map
                    (\f ->
                        renderPID renderActor systemModel
                            |> f pid
                    )
            )
