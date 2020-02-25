module System.Internal.Model exposing
    ( SystemModel
    , addAddress
    , addView
    , foldlInstances
    , getAddress
    , getChildren
    , getDocumentTitle
    , getInstance
    , getInstances
    , getNewPID
    , getViews
    , init
    , removeFromAddress
    , removeFromView
    , removePID
    , updateDocumentTitle
    , updateInstance
    )

import Dict
import System.Internal.PID exposing (PID(..), equals, system, toInt)


type alias SystemModelRecord applicationAddress applicationActorName componentModel =
    { instances : Dict.Dict Int ( PID, applicationActorName, componentModel )
    , children : Dict.Dict Int (List PID)
    , applicationAddresses : List ( applicationAddress, PID )
    , lastPID : PID
    , views : List PID
    , documentTitle : String
    }


type SystemModel applicationAddress applicationActorName componentModel
    = SystemModel (SystemModelRecord applicationAddress applicationActorName componentModel)


init : SystemModel applicationAddress applicationActorName componentModel
init =
    SystemModel
        { instances = Dict.empty
        , children = Dict.empty
        , applicationAddresses = []
        , lastPID = system
        , views = []
        , documentTitle = ""
        }


getInstance :
    PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> Maybe ( applicationActorName, componentModel )
getInstance pid (SystemModel { instances }) =
    Dict.get (toInt pid) instances
        |> Maybe.map
            (\( _, applicationActorName, componentModel ) ->
                ( applicationActorName, componentModel )
            )


getInstances :
    SystemModel applicationAddress applicationActorName componentModel
    -> List PID
getInstances (SystemModel { instances }) =
    Dict.toList instances
        |> List.map (\( _, ( pid, _, _ ) ) -> pid)


getChildren :
    PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> Maybe (List PID)
getChildren pid (SystemModel { children }) =
    Dict.get (toInt pid) children


getViews :
    SystemModel applicationAddress applicationActorName componentModel
    -> List PID
getViews (SystemModel { views }) =
    views


{-| Retrieve the document title
-}
getDocumentTitle :
    SystemModel applicationAddress applicationActorName componentModel
    -> String
getDocumentTitle (SystemModel { documentTitle }) =
    documentTitle


getAddress :
    applicationAddress
    -> SystemModel applicationAddress applicationActorName componentModel
    -> ( applicationAddress, List PID )
getAddress applicationAddress (SystemModel systemModelRecord) =
    List.foldl
        (\( xapplicationAddress, xpid ) ( _, listOfPids ) ->
            if xapplicationAddress == applicationAddress then
                ( applicationAddress, xpid :: listOfPids )

            else
                ( applicationAddress, listOfPids )
        )
        ( applicationAddress, [] )
        systemModelRecord.applicationAddresses


getNewPID :
    Maybe PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> ( PID, SystemModel applicationAddress applicationActorName componentModel )
getNewPID maybeSpawendBy (SystemModel ({ lastPID, children } as systemModelRecord)) =
    let
        spawnedBy =
            Maybe.withDefault system maybeSpawendBy

        spawnedByPidId =
            toInt spawnedBy

        pidId =
            toInt lastPID + 1

        pid =
            PID
                { id = pidId
                , spawnedBy = spawnedBy
                }

        updatedChildren =
            Dict.insert spawnedByPidId
                (Dict.get spawnedByPidId children
                    |> Maybe.map
                        (\setOfChildren ->
                            pid :: setOfChildren
                        )
                    |> Maybe.withDefault [ pid ]
                )
                children
    in
    ( pid
    , SystemModel
        { systemModelRecord
            | lastPID = pid
            , children = updatedChildren
        }
    )


updateInstance :
    PID
    -> applicationActorName
    -> SystemModel applicationAddress applicationActorName componentModel
    -> componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
updateInstance pid applicationActorName (SystemModel systemModelRecord) componentModel =
    SystemModel
        { systemModelRecord
            | instances =
                Dict.insert
                    (toInt pid)
                    ( pid, applicationActorName, componentModel )
                    systemModelRecord.instances
        }


updateDocumentTitle :
    String
    -> SystemModel applicationAddress applicationActorName componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
updateDocumentTitle documentTitle (SystemModel systemModelRecord) =
    SystemModel { systemModelRecord | documentTitle = documentTitle }


addView :
    PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
addView pid (SystemModel systemModelRecord) =
    SystemModel { systemModelRecord | views = pid :: systemModelRecord.views }


addAddress :
    applicationAddress
    -> PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
addAddress applicationAddress pid (SystemModel systemModelRecord) =
    SystemModel
        { systemModelRecord
            | applicationAddresses = ( applicationAddress, pid ) :: systemModelRecord.applicationAddresses
        }


removePID :
    PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
removePID pid (SystemModel systemModelRecord) =
    let
        pidId =
            toInt pid
    in
    SystemModel <|
        { systemModelRecord
            | instances = Dict.remove pidId systemModelRecord.instances
            , children =
                Dict.remove pidId systemModelRecord.children
                    |> Dict.map
                        (\_ a -> List.filter (not << equals pid) a)
            , views = List.filter (not << equals pid) systemModelRecord.views
            , applicationAddresses = List.filter (not << equals pid << Tuple.second) systemModelRecord.applicationAddresses
        }


removeFromView :
    PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
removeFromView pid (SystemModel systemModelRecord) =
    SystemModel
        { systemModelRecord
            | views =
                List.foldr
                    (\a r ->
                        if equals pid a then
                            r

                        else
                            a :: r
                    )
                    []
                    systemModelRecord.views
        }


removeFromAddress :
    applicationAddress
    -> PID
    -> SystemModel applicationAddress applicationActorName componentModel
    -> SystemModel applicationAddress applicationActorName componentModel
removeFromAddress applicationAddress pid (SystemModel systemModelRecord) =
    SystemModel
        { systemModelRecord
            | applicationAddresses =
                List.foldr
                    (\( a, b ) r ->
                        if a == applicationAddress && equals pid b then
                            r

                        else
                            ( a, b ) :: r
                    )
                    []
                    systemModelRecord.applicationAddresses
        }


foldlInstances :
    ({ applicationActorName : applicationActorName
     , componentModel :
        componentModel
     , pid : PID
     }
     -> List (Sub systemMessage)
     -> List (Sub systemMessage)
    )
    -> List (Sub systemMessage)
    -> SystemModel applicationAddress applicationActorName componentModel
    -> List (Sub systemMessage)
foldlInstances f initial (SystemModel { instances }) =
    Dict.foldl
        (\_ ( pid, applicationActorName, componentModel ) x ->
            f
                { pid = pid
                , applicationActorName = applicationActorName
                , componentModel = componentModel
                }
                x
        )
        initial
        instances
