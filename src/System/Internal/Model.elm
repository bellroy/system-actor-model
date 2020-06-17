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


type alias SystemModelRecord addresses actors appModel =
    { instances : Dict.Dict Int ( PID, actors, appModel )
    , children : Dict.Dict Int (List PID)
    , addresseses : List ( addresses, PID )
    , lastPID : PID
    , views : List PID
    , documentTitle : String
    }


type SystemModel addresses actors appModel
    = SystemModel (SystemModelRecord addresses actors appModel)


init : SystemModel addresses actors appModel
init =
    SystemModel
        { instances = Dict.empty
        , children = Dict.empty
        , addresseses = []
        , lastPID = system
        , views = []
        , documentTitle = ""
        }


getInstance :
    PID
    -> SystemModel addresses actors appModel
    -> Maybe ( actors, appModel )
getInstance pid (SystemModel { instances }) =
    Dict.get (toInt pid) instances
        |> Maybe.map
            (\( _, actors, appModel ) ->
                ( actors, appModel )
            )


getInstances :
    SystemModel addresses actors appModel
    -> List PID
getInstances (SystemModel { instances }) =
    Dict.toList instances
        |> List.map (\( _, ( pid, _, _ ) ) -> pid)


getChildren :
    PID
    -> SystemModel addresses actors appModel
    -> Maybe (List PID)
getChildren pid (SystemModel { children }) =
    Dict.get (toInt pid) children


getViews :
    SystemModel addresses actors appModel
    -> List PID
getViews (SystemModel { views }) =
    views


{-| Retrieve the document title
-}
getDocumentTitle :
    SystemModel addresses actors appModel
    -> String
getDocumentTitle (SystemModel { documentTitle }) =
    documentTitle


getAddress :
    addresses
    -> SystemModel addresses actors appModel
    -> ( addresses, List PID )
getAddress addresses (SystemModel systemModelRecord) =
    List.foldl
        (\( xaddresses, xpid ) ( _, listOfPids ) ->
            if xaddresses == addresses then
                ( addresses, xpid :: listOfPids )

            else
                ( addresses, listOfPids )
        )
        ( addresses, [] )
        systemModelRecord.addresseses


getNewPID :
    Maybe PID
    -> SystemModel addresses actors appModel
    -> ( PID, SystemModel addresses actors appModel )
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
    -> actors
    -> SystemModel addresses actors appModel
    -> appModel
    -> SystemModel addresses actors appModel
updateInstance pid actors (SystemModel systemModelRecord) appModel =
    SystemModel
        { systemModelRecord
            | instances =
                Dict.insert
                    (toInt pid)
                    ( pid, actors, appModel )
                    systemModelRecord.instances
        }


updateDocumentTitle :
    String
    -> SystemModel addresses actors appModel
    -> SystemModel addresses actors appModel
updateDocumentTitle documentTitle (SystemModel systemModelRecord) =
    SystemModel { systemModelRecord | documentTitle = documentTitle }


addView :
    PID
    -> SystemModel addresses actors appModel
    -> SystemModel addresses actors appModel
addView pid (SystemModel systemModelRecord) =
    SystemModel { systemModelRecord | views = pid :: systemModelRecord.views }


addAddress :
    addresses
    -> PID
    -> SystemModel addresses actors appModel
    -> SystemModel addresses actors appModel
addAddress addresses pid (SystemModel systemModelRecord) =
    SystemModel
        { systemModelRecord
            | addresseses = ( addresses, pid ) :: systemModelRecord.addresseses
        }


removePID :
    PID
    -> SystemModel addresses actors appModel
    -> SystemModel addresses actors appModel
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
            , addresseses = List.filter (not << equals pid << Tuple.second) systemModelRecord.addresseses
        }


removeFromView :
    PID
    -> SystemModel addresses actors appModel
    -> SystemModel addresses actors appModel
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
    addresses
    -> PID
    -> SystemModel addresses actors appModel
    -> SystemModel addresses actors appModel
removeFromAddress addresses pid (SystemModel systemModelRecord) =
    SystemModel
        { systemModelRecord
            | addresseses =
                List.foldr
                    (\( a, b ) r ->
                        if a == addresses && equals pid b then
                            r

                        else
                            ( a, b ) :: r
                    )
                    []
                    systemModelRecord.addresseses
        }


foldlInstances :
    ({ actors : actors
     , appModel :
        appModel
     , pid : PID
     }
     -> List (Sub systemMessage)
     -> List (Sub systemMessage)
    )
    -> List (Sub systemMessage)
    -> SystemModel addresses actors appModel
    -> List (Sub systemMessage)
foldlInstances f initial (SystemModel { instances }) =
    Dict.foldl
        (\_ ( pid, actors, appModel ) x ->
            f
                { pid = pid
                , actors = actors
                , appModel = appModel
                }
                x
        )
        initial
        instances
