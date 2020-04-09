module Main exposing (main)

import ActorName as ActorName exposing (ActorName(..))
import Address as Address exposing (Address(..))
import Bootstrap exposing (AppModel, bootstrap)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Json.Encode as Encode
import Msg as Msg exposing (AppMsg(..), Msg)
import System.Browser as Browser
import System.Log exposing (LogMessage)
import System.Message as SystemMessage exposing (SystemMessage)
import System.Platform exposing (Program)


main : Program () Address ActorName AppModel AppMsg
main =
    Browser.element
        { apply = bootstrap.apply
        , factory = bootstrap.factory
        , init = init
        , view = view
        , onLogMessage = onLogMessage
        }


init : () -> List Msg
init _ =
    [ SystemMessage.spawnWithFlags
        (Encode.int 1)
        ActorName.Counters
        SystemMessage.populateView
    , SystemMessage.spawn
        ActorName.Templating
        SystemMessage.populateView
    , SystemMessage.spawn
        ActorName.Snackbar
        (\pid ->
            SystemMessage.batch
                [ SystemMessage.populateView
                    pid
                , SystemMessage.populateAddress
                    Address.Snackbar
                    pid
                ]
        )
    ]


view : List (Html Msg) -> Html Msg
view contents =
    Html.div []
        [ Html.node "link"
            [ HtmlA.rel "stylesheet"
            , HtmlA.href "https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css"
            ]
            []
        , Html.node "style"
            []
            [ Html.text ".template-error { padding: 10px; background-color: #c00; color: #fff; }"
            ]
        , Html.div [ HtmlA.class "jumbotron" ]
            [ Html.div [ HtmlA.class "container" ]
                [ Html.h1 [ HtmlA.class "display-4" ]
                    [ Html.text "System Actor Model"
                    ]
                , Html.p [ HtmlA.class "lead" ]
                    [ Html.text "An Actor Model implementation for Elm"
                    ]
                , Html.hr [ HtmlA.class "my-4" ] []
                , Html.a
                    [ HtmlA.class "btn btn-primary btn-lg"
                    , HtmlA.href "https://package.elm-lang.org/packages/tricycle/system-actor-model/latest"
                    ]
                    [ Html.text "documentation" ]
                , Html.text " "
                , Html.a
                    [ HtmlA.class "btn btn-secondary btn-lg"
                    , HtmlA.href "https://github.com/tricycle/system-actor-model"
                    ]
                    [ Html.text "source" ]
                ]
            ]
        , Html.div [ HtmlA.class "container" ] contents
        ]


onLogMessage :
    LogMessage Address ActorName AppMsg
    -> SystemMessage Address ActorName AppMsg
onLogMessage =
    Msg.LogMsg
        >> SystemMessage.sendToAddress
            Address.Snackbar
