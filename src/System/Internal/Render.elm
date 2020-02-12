module System.Internal.Render exposing
    ( renderPID
    , view
    , viewApplication
    )

import Browser
import Html exposing (Html)
import System.Internal.Message exposing (Message(..))
import System.Internal.Model exposing (Model, getDocumentTitle, getInstance, getViews)
import System.Internal.PID exposing (PID)
import System.Internal.SystemActor exposing (SystemActor(..))


view :
    { a
        | apply :
            actorModel
            -> SystemActor actorModel output (Message address actorName appMsg)
    }
    -> Model address actorName actorModel
    -> List output
view { apply } model =
    List.filterMap
        (renderPID
            (\processModel ->
                let
                    (SystemActor systemActor) =
                        apply processModel
                in
                systemActor.view
            )
            model
        )
    <|
        List.reverse (getViews model)


viewApplication :
    { a
        | apply :
            actorModel -> SystemActor actorModel output (Message address actorName appMsg)
        , view :
            List output -> List (Html (Message address actorName appMsg))
    }
    -> Model address actorName actorModel
    -> Browser.Document (Message address actorName appMsg)
viewApplication impl model =
    view impl model
        |> impl.view
        |> (\body ->
                { title = getDocumentTitle model
                , body = body
                }
           )


renderPID :
    (actorModel
     -> PID
     -> (PID -> Maybe output)
     -> output
    )
    -> Model address actorName actorModel
    -> PID
    -> Maybe output
renderPID renderActor model pid =
    getInstance pid model
        |> Maybe.map
            (\( _, actorModel ) ->
                renderActor
                    actorModel
                    pid
                    (renderPID renderActor model)
            )
