module System.Html.Template exposing
    ( HtmlTemplate, HtmlComponentId, empty, parse, decode
    , SpawnableHtmlComponent, HtmlComponentFactory, htmlComponentFactory, toListSpawnableHtmlComponents
    , render
    , spawn
    , encode, encodeSpawnableHtmlComponent
    )

{-| This module can be used to easily outfit your application with a templating system that understands
the System Actor Model and can return a List of ActorNames from your Application to spawn and interpolate.

Quick Example

    import Dict
    import System.Component.Layout exposing (Layout)
    import System.Event exposing (systemDefault)
    import System.Html.Template as HtmlTemplate
    import System.Process exposing (PID)

    type ActorName
        = Counter

    type Address
        = Counters

    type MsgIn
        = SpawnedComponent String PID

    type alias Model =
        { instances : Dict String PID
        , HtmlTemplate: HtmlTemplate ActorName Address
        }

    {-| This component is a [Layout](/packages/tricycle/system-actor-model/latest/System-Component-Layout).
    It would otherwise not be able to render other Actors using their PID. -}
    component : Layout Model MsgIn MsgOut msg
    component =
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , events = systemDefault
        , view = view
        }


    htmlComponents = Dict HtmlTemplate.HtmlComponentId HtmlTemplate.HtmlComponentFactory ActorName Address
    htmlComponents =
        Dict.fromList
            [ HtmlTemplate.htmlComponentFactory
                { prefix = "app"
                , name = "counter"
                , actorName = ActorName.Counter
                , addresses = []
                , defaultAttributes = [ ( "value", Encode.int 0 ) ]
                , requiredAtributes = [ "value" ]
                }
            ]

    template: String
    template =
        """
        <div class="Counters">
            <prefix-counter></prefix-counter>
            <prefix-counter value="10">
                <h2>I start at 10!</h2>
            </prefix-counter>
        </div>
        """

    init : a -> ( Model, List MsgOut, Cmd MsgIn )
    init _ =
        case HtmlTemplate.parse htmlComponents template of
            Err _ ->
               --  Handle error

            Ok htmlTemplate ->
                ( Model Dict.empty htmlTemplate
                , [ HtmlTempalte.toListSpawnableHtmlComponents htmlTemplate
                        |> SpawnComponents
                  ]
                , Cmd.none
                )

    update : MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
    update msgIn model =
        case msgIn of
            SpawnedComponent htmlComponentId pid ->
                Dict.insert htmlComponentId pid model.components



    view : (MsgIn -> msg)
        -> Model
        -> (PID -> Html msg)
        -> Html msg
    view _ model renderPid =

        HtmlTemplate.render {
            renderPid = renderPid
            , instances = instances
            , interpolate = Dict.empty
            , htmlTemplate = model.htmlTemplate
            }
                |> div []


# Template

@docs HtmlTemplate, HtmlComponentId, empty, parse, decode


# Html Components

@docs SpawnableHtmlComponent, HtmlComponentFactory, htmlComponentFactory, toListSpawnableHtmlComponents


# Rendering

@docs render


# Spawning

@docs spawn


# Encoding

@docs encode, encodeSpawnableHtmlComponent

-}

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode exposing (Decoder)
import Json.Encode exposing (Value)
import System.Html.Internal.Template as Internal
    exposing
        ( HtmlComponentFactory
        , HtmlTemplate
        , SpawnableHtmlComponent
        , decode
        , empty
        , encode
        , encodeSpawnableHtmlComponent
        , render
        , toListSpawnableHtmlComponents
        )
import System.Internal.Message exposing (SystemMessage)
import System.Internal.PID exposing (PID)


{-| The (opaque) type of HtmlTemplate represents a parsed Template within your application.
-}
type alias HtmlTemplate actorName address =
    Internal.HtmlTemplate actorName address


{-| A HtmlTemplate is identified by a String that is unique for component and its attributes

This means that a htmlComponent with the exact same name and attributes will not be spwaned multiple times so that you can fully leverage the System Actor Model.
In the case where you end up with the need to have two different Actors with the same starting attributes
(in the case they request additional data or in the case of two counters that should indiviually run)
you can simply pass an unique attribute from whitin your app.

-}
type alias HtmlComponentId =
    String


{-| Create an empty HtmlTemplate
-}
empty : HtmlTemplate actorName address
empty =
    Internal.empty


{-| Parse a HtmlTemplate from a String with given available htmlComponents
-}
parse :
    Dict HtmlComponentId (HtmlComponentFactory actorName address)
    -> String
    -> Result String (HtmlTemplate actorName address)
parse =
    Internal.parse


{-| Decode an Encoded string of Html nodes into a HtmlTemplate
-}
decode :
    Dict HtmlComponentId (HtmlComponentFactory actorName address)
    -> Decoder (HtmlTemplate actorName address)
decode =
    Internal.decode


{-| A SpawnableHtmlComponent represents a component as how it was found whitin your suplied HtmlTemplate.
It is up to your application to [spawn](#spawn)
this component using the actorName provided and storing it inside your model with the id found on the
SpawnableHtmlComponent record
-}
type alias SpawnableHtmlComponent actorName address =
    { actorName : actorName
    , addresses : List address
    , attributes : List ( String, Value )
    , htmlTemplate : HtmlTemplate actorName address
    , id : String
    , nodeName : String
    }


{-| The (opaque) type of HtmlComponentFactory represents a set of configuration options that are required to
identify and parse your components from a HtmlTemplate.
-}
type alias HtmlComponentFactory actorName address =
    Internal.HtmlComponentFactory actorName address


{-| Create an HtmlComponentFactory

  - prefix. HtmlComponents are designed to be valid Html5 tags. Custom components require a prefix.

  - name. Together with prefix this dictates the final name of your HtmlComponent

        { config | prefix = "foo", name = "bar" }

    will create a HtmlComponent that can be invoked by entering `<foo-bar></foo-bar>` in your template.

  - actorName. The actorName of the Actor that should be spawned when this component is encountered.

  - address. Optionally supply an Address under which this component should be spawned.

  - requiredAttributes. You can supply a list of attribute names that are required.
    When one of these attributes is found missing an Error message will be displayed instead.

  - defaultAttributes. Supply a List of default values for your attributes.

-}
htmlComponentFactory :
    { prefix : String
    , name : String
    , actorName : actorName
    , addresses : List address
    , requiredAtributes : List String
    , defaultAttributes : List ( String, Value )
    }
    -> ( HtmlComponentId, HtmlComponentFactory actorName address )
htmlComponentFactory configuration =
    -- ( name
    -- , Internal.htmlComponentFactory
    --     { name = name
    --     , actor = actorName
    --     , address = address
    --     , config = config
    --     }
    -- )
    ( configuration.prefix ++ "-" ++ configuration.name
    , Internal.htmlComponentFactory configuration
    )


{-| Retrieve a list of spawnableHtmlComponent from a given HtmlTemplate
-}
toListSpawnableHtmlComponents :
    HtmlTemplate actorName address
    -> List (SpawnableHtmlComponent actorName address)
toListSpawnableHtmlComponents =
    Internal.toListSpawnableHtmlComponents


{-| Render
-}
render :
    { renderPid : pid -> Html msg
    , instances : Dict String pid
    , interpolate : Dict String String
    , htmlTemplate : HtmlTemplate actorName address
    }
    -> List (Html.Html msg)
render =
    Internal.render


{-| Spawn a SpawnableHtmlComponent

This is a helper function, the same can be achieved by using the [System.Message.spawn](/packages/tricycle/system-actor-model/latest/System-Message#spawn) function

-}
spawn :
    (HtmlComponentId -> PID -> SystemMessage address actorName appMsg)
    -> SpawnableHtmlComponent actorName address
    -> SystemMessage address actorName appMsg
spawn =
    Internal.spawn


{-| Encode a HtmlTemplate into an Encoded Value
-}
encode :
    HtmlTemplate actorName address
    -> Value
encode =
    Internal.encode


{-| Encode a SpawnableHtmlComponent into an Encoded Value
-}
encodeSpawnableHtmlComponent : SpawnableHtmlComponent actorName address -> Value
encodeSpawnableHtmlComponent =
    Internal.encodeSpawnableHtmlComponent
