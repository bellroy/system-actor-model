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
    List.filterMap
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
    <|
        List.reverse (SystemModel.getViews systemModel)


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


renderPID :
    (componentModel
     -> PID
     ->
        (PID
         -> Maybe componentOutput
        )
     -> componentOutput
    )
    -> SystemModel applicationAddress applicationActorName componentModel
    -> PID
    -> Maybe componentOutput
renderPID renderActor systemModel pid =
    SystemModel.getInstance pid systemModel
        |> Maybe.map
            (\( _, componentModel ) ->
                renderPID renderActor systemModel
                    |> renderActor componentModel pid
            )
