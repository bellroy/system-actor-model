module Main exposing (main)

import ActorName as ActorName exposing (ActorName(..))
import Address as Address exposing (Address(..))
import Bootstrap exposing (AppModel, bootstrap)
import Components.Snackbar as Snackbar exposing (MsgIn(..))
import Html exposing (Html, a, div, h1, node, p, text)
import Html.Attributes exposing (class, href, rel)
import Json.Encode as Encode
import Msg as Msg exposing (AppMsg(..), Msg)
import System.Browser exposing (element)
import System.Log exposing (LogMessage, toString)
import System.Message exposing (..)
import System.Platform exposing (Program)


main : Program () Address ActorName AppModel AppMsg
main =
    element
        { apply = bootstrap.apply
        , factory = bootstrap.factory
        , init = init
        , view = view
        , onLogMessage = onLogMessage
        }


init : () -> List Msg
init _ =
    [ spawnWithFlags (Encode.int 1) ActorName.Counters populateView
    , spawn ActorName.Snackbar
        (\pid ->
            batch
                [ populateView pid
                , populateAddress Address.Snackbar pid
                ]
        )
    ]


view : List (Html Msg) -> Html Msg
view contents =
    div []
        [ node "link" [ rel "stylesheet", href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css" ] []
        , div [ class "jumbotron" ]
            [ div [ class "container" ]
                [ h1 [ class "display-4" ] [ text "System Actor Model Demo" ]
                , p [ class "lead" ] [ text "An Actor Model implementation for Elm" ]
                , node "hr" [ class "my-4" ] []
                , p []
                    [ text "Check out the Demo below or jump to Source or Documentation." ]
                , a
                    [ class "btn btn-primary btn-lg", href "https://package.elm-lang.org/packages/tricycle/system-actor-model/latest" ]
                    [ text "documentation" ]
                , text " "
                , a [ class "btn btn-secondary btn-lg", href "https://github.com/tricycle/system-actor-model" ] [ text "source" ]
                ]
            ]
        , div [ class "container" ] contents
        ]


onLogMessage :
    LogMessage Address ActorName AppMsg
    -> SystemMessage Address ActorName AppMsg
onLogMessage =
    sendToAddress Address.Snackbar
        << Msg.LogMsg
