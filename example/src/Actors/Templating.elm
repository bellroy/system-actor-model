module Actors.Templating exposing (Model, actor)

import ActorName exposing (ActorName(..))
import Address exposing (Address)
import Components.Templating as Templating
import Dict exposing (Dict)
import Html exposing (Html)
import Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Layout as Layout
import System.Html.Template as HtmlTemplate exposing (HtmlComponentFactory, HtmlComponentId)
import System.Message as SystemMessage
import System.Process exposing (PID)


type alias Model =
    Templating.Model ActorName Address


type alias MsgIn =
    Templating.MsgIn


type alias MsgOut =
    Templating.MsgOut ActorName Address


htmlComponents : Dict HtmlComponentId (HtmlComponentFactory ActorName Address)
htmlComponents =
    Dict.fromList
        [ HtmlTemplate.htmlComponentFactory
            { prefix = "app"
            , name = "counter"
            , actorName = ActorName.Counter
            , addresses = []
            , defaultAttributes = []
            , requiredAtributes = [ "value" ]
            }
        ]


actor :
    (Model -> appModel)
    -> Actor Model appModel (Html Msg) Msg
actor wrapModel =
    Layout.toActor
        (Templating.component htmlComponents)
        { wrapModel = wrapModel
        , wrapMsg = Msg.Templating
        , mapIn = mapIn
        , mapOut = mapOut
        }


mapIn :
    Msg.AppMsg
    -> Maybe MsgIn
mapIn appMsg =
    case appMsg of
        Msg.Templating msgIn ->
            Just msgIn

        _ ->
            Nothing


mapOut :
    PID
    -> MsgOut
    -> Msg
mapOut self msgOut =
    case msgOut of
        Templating.SpawnNewTemplateComponents oldInstances htmlTemplate ->
            let
                killMsgs =
                    Dict.toList oldInstances
                        |> List.map
                            (Tuple.second
                                >> SystemMessage.kill
                            )

                spawnComponentMsgs =
                    HtmlTemplate.toListSpawnableHtmlComponents htmlTemplate
                        |> List.map
                            (HtmlTemplate.spawn
                                (\instanceId instancePid ->
                                    Templating.OnSpawnedComponent instanceId instancePid
                                        |> Msg.Templating
                                        |> SystemMessage.sendToPid self
                                )
                            )
            in
            killMsgs
                ++ spawnComponentMsgs
                |> SystemMessage.batch
