module Components.Templating exposing (Model, MsgIn(..), MsgOut(..), component)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import System.Component.Layout exposing (Layout)
import System.Event as SystemEvent
import System.Html.Template as HtmlTemplate
import System.Process as PID exposing (PID)


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
    -> Layout (Html msg) (Model actorName address) MsgIn (MsgOut actorName address) msg
component htmlComponents =
    { init = init htmlComponents
    , update = update
    , view = view
    , subscriptions = always Sub.none
    , events = SystemEvent.ignoreAll
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
                    ( { model
                        | error =
                            Just "There was an error parsing your Html Template"
                      }
                    , []
                    , Cmd.none
                    )

        OnSpawnedComponent htmlComponentId pid ->
            ( { model
                | instances =
                    Dict.insert
                        htmlComponentId
                        pid
                        model.instances
              }
            , []
            , Cmd.none
            )


view :
    (MsgIn -> msg)
    -> Model actorName address
    -> (PID -> Maybe (Html msg))
    -> Html msg
view toSelf model renderPid =
    Html.div []
        [ Html.h2 []
            [ Html.text ("Templating (PID: " ++ PID.pidToString model.pid ++ ")")
            ]
        , Html.div [ HtmlA.class "form-group" ]
            [ Html.label
                [ HtmlA.for "inputTemplate"
                ]
                [ Html.text "Template Input" ]
            , Html.map toSelf <|
                Html.textarea
                    [ HtmlA.class "form-control"
                    , HtmlA.id "inputTemplate"
                    , HtmlA.rows
                        (String.lines model.inputTemplate
                            |> List.length
                            |> (+) 1
                        )
                    , HtmlE.onInput OnNewTemplate
                    ]
                    [ Html.text model.inputTemplate ]
            ]
        , Html.hr [ HtmlA.class "my-4" ] []
        , case model.error of
            Just errorString ->
                Html.div [ HtmlA.class "alert alert-danger" ]
                    [ Html.text errorString
                    ]

            Nothing ->
                Html.text ""
        , HtmlTemplate.render
            { renderPid =
                renderPid
                    >> Maybe.withDefault (Html.text "")
            , instances = model.instances
            , interpolate = Dict.empty
            , htmlTemplate = model.htmlTemplate
            }
            |> Html.div
                [ HtmlA.class "container"
                , HtmlA.style "background-color" "#f5f5f5"
                ]
        , Html.div [ HtmlA.style "margin-bottom" "100px" ] []
        ]
