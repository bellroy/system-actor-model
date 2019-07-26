module Main exposing (main)

-- import Components.Layout.Pages.Msg as ComponentLayoutPages exposing (MsgIn(..))
-- import Components.Worker.Router as ComponentWorkerRouter exposing (MsgIn(..))

import ActorName as Actor exposing (ActorName(..))
import Address exposing (Address(..))
import Bootstrap exposing (AppModel, bootstrap)
import Html exposing (Html, div)
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
    [ spawn Actor.Counters populateView ]


view : List (Html Msg) -> Html Msg
view =
    div []
