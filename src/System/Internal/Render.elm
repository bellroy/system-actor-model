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
        | apply : appModel -> SystemActor appModel output (SystemMessage addresses actors appMsg)
    }
    -> SystemModel addresses actors appModel
    -> List output
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
        | apply : appModel -> SystemActor appModel output (SystemMessage addresses actors appMsg)
        , view : List output -> List (Html (SystemMessage addresses actors appMsg))
    }
    -> SystemModel addresses actors appModel
    -> Document (SystemMessage addresses actors appMsg)
viewApplication impl systemModel =
    view impl systemModel
        |> impl.view
        |> (\body ->
                { title = SystemModel.getDocumentTitle systemModel
                , body = body
                }
           )


renderPID :
    (appModel
     -> Maybe (PID -> (PID -> Maybe output) -> output)
    )
    -> SystemModel addresses actors appModel
    -> PID
    -> Maybe output
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
