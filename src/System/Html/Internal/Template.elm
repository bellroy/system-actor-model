module System.Html.Internal.Template exposing
    ( HtmlComponent
    , HtmlComponentFactory
    , HtmlTemplate
    , SpawnableHtmlComponent
    , decode
    , empty
    , encode
    , encodeSpawnableHtmlComponent
    , htmlComponentFactory
    , parse
    , render
    , spawn
    , toListSpawnableHtmlComponents
    )

import Dict
import Html
import Html.Attributes as HtmlA
import Html.Parser as HtmlParser
import Json.Decode as Decode
import Json.Encode as Encode
import MD5
import Parser exposing (deadEndsToString)
import System.Internal.Message exposing (Control(..), SystemMessage(..))
import System.Internal.PID exposing (PID)
import System.Message as SystemMessage


type HtmlTemplate actorName address
    = HtmlTemplate (List (TemplateElement actorName address))


type TemplateElement actorName address
    = Text String
    | Error String
    | Node String (List ( String, String )) (HtmlTemplate actorName address)
    | Component (HtmlComponent actorName address)


type HtmlComponent actorName address
    = HtmlComponent
        { actorName : actorName
        , address : Maybe address
        , attributes : List ( String, Encode.Value )
        , htmlTemplate : HtmlTemplate actorName address
        , id : String
        , nodeName : String
        }


type alias SpawnableHtmlComponent actorName address =
    { actorName : actorName
    , address : Maybe address
    , attributes : List ( String, Encode.Value )
    , htmlTemplate : HtmlTemplate actorName address
    , id : String
    , nodeName : String
    }


type alias HtmlComponentFactory actorName address =
    List ( String, Encode.Value )
    -> HtmlTemplate actorName address
    -> Result String (HtmlComponent actorName address)


type alias HtmlComponentId =
    String


htmlComponentFactory :
    { prefix : String
    , name : String
    , actorName : actorName
    , address : Maybe address
    , requiredAtributes : List String
    , defaultAttributes : List ( String, Encode.Value )
    }
    -> List ( String, Encode.Value )
    -> HtmlTemplate actorName address
    -> Result String (HtmlComponent actorName address)
htmlComponentFactory configuration attributes children =
    let
        nodeName =
            configuration.prefix ++ "-" ++ configuration.name

        attributesAsObject =
            List.concat
                [ configuration.defaultAttributes
                , attributes
                ]
                |> Encode.object

        htmlComponentIdHash =
            [ nodeName
            , Encode.encode 0 attributesAsObject
            , htmlTemplateToString children
            ]
                |> List.map MD5.hex
                |> String.join ":"

        suppliedAttributes =
            configuration.defaultAttributes ++ attributes

        suppliedAttributeKeys =
            List.map Tuple.first suppliedAttributes

        missingAttributeErrors =
            List.filterMap
                (\a ->
                    if List.member a suppliedAttributeKeys then
                        Nothing

                    else
                        Just <| "Error: Missing attribute \"" ++ a ++ "\" on node \"" ++ nodeName ++ "\"."
                )
                configuration.requiredAtributes

        htmlComponentAttributes =
            [ ( "data-x-name", Encode.string nodeName )
            , ( "data-x-id", Encode.string htmlComponentIdHash )
            ]
                ++ suppliedAttributes
    in
    if List.length missingAttributeErrors > 0 then
        Err (String.join " " missingAttributeErrors)

    else
        Ok <|
            HtmlComponent
                { actorName = configuration.actorName
                , address = configuration.address
                , attributes = htmlComponentAttributes
                , htmlTemplate = children
                , id = htmlComponentIdHash
                , nodeName = nodeName
                }


decode :
    Dict.Dict String (HtmlComponentFactory actorName address)
    -> Decode.Decoder (HtmlTemplate actorName address)
decode htmlComponents =
    Decode.andThen
        (\a ->
            case parse htmlComponents a of
                Ok htmlTemplate ->
                    Decode.succeed htmlTemplate

                Err parseError ->
                    Decode.fail parseError
        )
        Decode.string


empty : HtmlTemplate actorName address
empty =
    HtmlTemplate []


parse :
    Dict.Dict String (HtmlComponentFactory actorName address)
    -> String
    -> Result String (HtmlTemplate actorName address)
parse htmlComponents =
    Result.map (htmlParserNodesToHtmlTemplate htmlComponents)
        << Result.mapError deadEndsToString
        << HtmlParser.run


htmlParserNodesToHtmlTemplate :
    Dict.Dict String (HtmlComponentFactory actorName address)
    -> List HtmlParser.Node
    -> HtmlTemplate actorName address
htmlParserNodesToHtmlTemplate htmlComponents =
    List.filterMap (htmlParserNodeToTemplateElement htmlComponents)
        >> HtmlTemplate


htmlParserNodeToTemplateElement :
    Dict.Dict String (HtmlComponentFactory actorName address)
    -> HtmlParser.Node
    -> Maybe (TemplateElement actorName address)
htmlParserNodeToTemplateElement htmlComponents parserNode =
    case parserNode of
        HtmlParser.Comment _ ->
            Nothing

        HtmlParser.Text "" ->
            Nothing

        HtmlParser.Text a ->
            if String.trim a == "" then
                Nothing

            else
                Just <| Text a

        HtmlParser.Element nodeName domAttributes children ->
            let
                htmlTemplate =
                    htmlParserNodesToHtmlTemplate htmlComponents children
            in
            case Dict.get nodeName htmlComponents of
                Just factory ->
                    let
                        factoryResult =
                            factory
                                (List.map (\( k, v ) -> ( k, Encode.string v )) domAttributes)
                                htmlTemplate
                    in
                    case factoryResult of
                        Ok htmlComponent ->
                            Just <| Component htmlComponent

                        Err err ->
                            Just <| Error err

                Nothing ->
                    Just <| Node nodeName domAttributes htmlTemplate


htmlTemplateToString :
    HtmlTemplate actorName address
    -> String
htmlTemplateToString (HtmlTemplate templateElements) =
    templateElementsToString templateElements


encode :
    HtmlTemplate actorName address
    -> Encode.Value
encode =
    htmlTemplateToString
        >> Encode.string


encodeSpawnableHtmlComponent :
    SpawnableHtmlComponent actorName address
    -> Encode.Value
encodeSpawnableHtmlComponent { attributes, htmlTemplate } =
    Encode.object
        [ ( "attributes", Encode.object attributes )
        , ( "htmlTemplate", encode htmlTemplate )
        ]


templateElementsToString :
    List (TemplateElement actorName address)
    -> String
templateElementsToString =
    List.map templateElementToString
        >> String.join "\n"


templateElementToString :
    TemplateElement actorName address
    -> String
templateElementToString =
    HtmlParser.nodeToString << toHtmlParserNode


toHtmlParserNode :
    TemplateElement actorName address
    -> HtmlParser.Node
toHtmlParserNode templateElement =
    case templateElement of
        Text string ->
            HtmlParser.Text string

        Error string ->
            HtmlParser.Element "div"
                [ ( "class", "template-error" ) ]
                [ HtmlParser.Text string
                ]

        Node nodeName domAttributes (HtmlTemplate listOfTemplateElements) ->
            List.map toHtmlParserNode listOfTemplateElements
                |> HtmlParser.Element nodeName domAttributes

        Component (HtmlComponent htmlComponent) ->
            let
                (HtmlTemplate listOfTemplateElements) =
                    htmlComponent.htmlTemplate

                domAttributes =
                    List.filterMap
                        (\( key, value ) ->
                            case Decode.decodeValue Decode.string value of
                                Ok string ->
                                    Just ( key, string )

                                Err _ ->
                                    Nothing
                        )
                        htmlComponent.attributes
            in
            List.map toHtmlParserNode listOfTemplateElements
                |> HtmlParser.Element htmlComponent.nodeName domAttributes


render :
    { renderPid : pid -> Html.Html msg
    , instances : Dict.Dict String pid
    , interpolate : Dict.Dict String String
    , htmlTemplate : HtmlTemplate actorName address
    }
    -> List (Html.Html msg)
render { renderPid, instances, interpolate, htmlTemplate } =
    let
        (HtmlTemplate listOfTemplateElements) =
            htmlTemplate
    in
    List.map
        (templateElementToHtml renderPid instances interpolate)
        listOfTemplateElements


templateElementToHtml :
    (pid -> Html.Html msg)
    -> Dict.Dict String pid
    -> Dict.Dict String String
    -> TemplateElement actorName address
    -> Html.Html msg
templateElementToHtml renderPid instances interpolate templateElement =
    let
        fprint string =
            List.foldl
                (\( k, v ) ->
                    String.replace k v
                )
                string
                (Dict.toList interpolate)
    in
    case templateElement of
        Text string ->
            fprint string
                |> Html.text

        Error string ->
            Html.div [ HtmlA.class "template-error" ]
                [ Html.text string
                ]

        Node nodeName domAttributes htmlTemplate ->
            render
                { renderPid = renderPid
                , instances = instances
                , interpolate = interpolate
                , htmlTemplate = htmlTemplate
                }
                |> Html.node nodeName
                    (List.map
                        (\( k, v ) ->
                            HtmlA.attribute k <| fprint v
                        )
                        domAttributes
                    )

        Component (HtmlComponent htmlComponent) ->
            Dict.get htmlComponent.id instances
                |> Maybe.map renderPid
                |> Maybe.withDefault
                    (Html.div [ HtmlA.class "template-error" ]
                        [ Html.text <| "Unable to find component with the id \"" ++ htmlComponent.id ++ "\" on the supplied instances Dict."
                        ]
                    )


htmlTemplateToSpawnableHtmlComponentsDict :
    HtmlTemplate actorName address
    -> Dict.Dict String (SpawnableHtmlComponent actorName address)
htmlTemplateToSpawnableHtmlComponentsDict (HtmlTemplate listOfTemplateElements) =
    listOfTemplateElementsToSpawnableHtmlComponentsDict Dict.empty listOfTemplateElements


listOfTemplateElementsToSpawnableHtmlComponentsDict :
    Dict.Dict String (SpawnableHtmlComponent actorName address)
    -> List (TemplateElement actorName address)
    -> Dict.Dict String (SpawnableHtmlComponent actorName address)
listOfTemplateElementsToSpawnableHtmlComponentsDict =
    List.foldl
        (\templateElement dict ->
            case templateElement of
                Component (HtmlComponent htmlComponent) ->
                    Dict.insert
                        htmlComponent.id
                        htmlComponent
                        dict

                Node _ _ (HtmlTemplate listOfTemplateElementsOnNode) ->
                    listOfTemplateElementsToSpawnableHtmlComponentsDict
                        dict
                        listOfTemplateElementsOnNode

                _ ->
                    dict
        )


toListSpawnableHtmlComponents :
    HtmlTemplate actorName address
    -> List (SpawnableHtmlComponent actorName address)
toListSpawnableHtmlComponents =
    List.map Tuple.second
        << Dict.toList
        << htmlTemplateToSpawnableHtmlComponentsDict


spawn :
    (HtmlComponentId -> PID -> SystemMessage address actorName applicationMessage)
    -> SpawnableHtmlComponent actorName address
    -> SystemMessage address actorName applicationMessage
spawn callback spawnableHtmlComponent =
    let
        flags =
            encodeSpawnableHtmlComponent spawnableHtmlComponent
    in
    SystemMessage.spawnWithFlags flags
        spawnableHtmlComponent.actorName
        (\pid ->
            case spawnableHtmlComponent.address of
                Just address ->
                    SystemMessage.batch
                        [ SystemMessage.populateAddress address pid
                        , callback spawnableHtmlComponent.id pid
                        ]

                Nothing ->
                    callback spawnableHtmlComponent.id pid
        )
