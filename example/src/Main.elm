module Main exposing (main)

import ActorName as Actor exposing (ActorName(..))
import Address exposing (Address(..))
import Bootstrap exposing (AppModel, bootstrap)
import Html exposing (Html, a, div, h1, p, text)
import Html.Attributes exposing (href)
import Json.Encode as Encode
import Msg exposing (AppMsg(..), Msg)
import System.Browser exposing (element)
import System.Message exposing (..)
import System.Platform exposing (Program)


main : Program () Address ActorName AppModel AppMsg
main =
    element
        { apply = bootstrap.apply
        , factory = bootstrap.factory
        , init = init
        , view = view
        }


init : () -> List Msg
init _ =
    [ spawnWithFlags
        (Encode.int 10)
        Actor.Counters
        populateView
    ]


view : List (Html Msg) -> Html Msg
view contents =
    div []
        [ h1 [] [ text "System Actor Model Demo" ]
        , p []
            [ a [ href "https://github.com/tricycle/system-actor-model" ] [ text "source" ]
            , text " "
            , a [ href "https://package.elm-lang.org/packages/tricycle/system-actor-model/latest" ] [ text "documentation" ]
            ]
        , div [] contents
        ]
