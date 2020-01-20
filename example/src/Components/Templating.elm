module Components.Templating exposing (Model, MsgIn(..), MsgOut(..), component)

import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import System.Component.Layout exposing (Layout)
import System.Event exposing (ignoreAll)
import System.Html.Template as HtmlTemplate
import System.Process exposing (PID, equals, pidToString)


type alias Instances =
    Dict HtmlTemplate.HtmlComponentId PID


type alias OldInstances =
    Instances


type alias Model actorName address =
    { pid : PID
    , inputTemplate : String
    , htmlComponents : Dict HtmlTemplate.HtmlComponentId (HtmlTemplate.HtmlComponentFactory actorName address)
    , htmlTemplate : HtmlTemplate.HtmlTemplate actorName address
    , instances : Instances
    , error : Maybe String
    }


type MsgIn
    = OnNewTemplate String
    | OnSpawnedComponent HtmlTemplate.HtmlComponentId PID


type MsgOut actorName address
    = SpawnNewTemplateComponents OldInstances (HtmlTemplate.HtmlTemplate actorName address)


startingTemplate : String
startingTemplate =
    """<h2>This is part of the template</h2>
<p>Note that the app-counter component doesn't render any enclosed Html for this example. <br />However this would be perfectly possible to achieve in your own application.</p>
<table class="table">
    <thead>
        <tr><th>PID</th><th>Count</th><th></th></tr>
    </thead>
    <tbody>
        <app-counter value="10"></app-counter>
        <app-counter value="100" steps="10"></app-counter>
    </tbody>
</table>
"""


component :
    Dict HtmlTemplate.HtmlComponentId (HtmlTemplate.HtmlComponentFactory actorName address)
    -> Layout (Model actorName address) MsgIn (MsgOut actorName address) msg
component htmlComponents =
    { init = init htmlComponents
    , update = update
    , view = view
    , subscriptions = always Sub.none
    , events = ignoreAll
    }


init :
    Dict HtmlTemplate.HtmlComponentId (HtmlTemplate.HtmlComponentFactory actorName address)
    -> ( PID, a )
    -> ( Model actorName address, List (MsgOut actorName address), Cmd MsgIn )
init htmlComponents ( pid, _ ) =
    update (OnNewTemplate startingTemplate)
        { pid = pid
        , inputTemplate = ""
        , htmlComponents = htmlComponents
        , htmlTemplate = HtmlTemplate.empty
        , instances = Dict.empty
        , error = Nothing
        }


update :
    MsgIn
    -> Model actorName address
    -> ( Model actorName address, List (MsgOut actorName address), Cmd MsgIn )
update msgIn model =
    case msgIn of
        OnNewTemplate inputTemplate ->
            case HtmlTemplate.parse model.htmlComponents inputTemplate of
                Ok htmlTemplate ->
                    ( { model
                        | htmlTemplate = htmlTemplate
                        , instances = Dict.empty
                        , inputTemplate = inputTemplate
                        , error = Nothing
                      }
                    , [ SpawnNewTemplateComponents model.instances htmlTemplate
                      ]
                    , Cmd.none
                    )

                Err _ ->
                    ( { model | error = Just "There was an error parsing your Html Template" }
                    , []
                    , Cmd.none
                    )

        OnSpawnedComponent htmlComponentId pid ->
            ( { model | instances = Dict.insert htmlComponentId pid model.instances }
            , []
            , Cmd.none
            )


view :
    (MsgIn -> msg)
    -> Model actorName address
    -> (PID -> Html msg)
    -> Html msg
view toSelf model renderPid =
    div []
        [ h2 [] [ text ("Templating (PID: " ++ pidToString model.pid ++ ")") ]
        , div [ class "form-group" ]
            [ label
                [ for "inputTemplate"
                ]
                [ text "Template Input" ]
            , Html.map toSelf <|
                textarea
                    [ class "form-control"
                    , id "inputTemplate"
                    , rows ((+) 1 <| List.length <| String.lines model.inputTemplate)
                    , onInput OnNewTemplate
                    ]
                    [ text model.inputTemplate ]
            ]
        , hr [ class "my-4" ] []
        , case model.error of
            Just errorString ->
                div [ class "alert alert-danger" ] [ text errorString ]

            Nothing ->
                text ""
        , HtmlTemplate.render
            { renderPid = renderPid
            , instances = model.instances
            , interpolate = Dict.empty
            , htmlTemplate = model.htmlTemplate
            }
            |> div [ class "container", style "background-color" "#f5f5f5" ]
        , div [ style "margin-bottom" "100px" ] []
        ]
