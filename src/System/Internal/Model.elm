module System.Internal.Model exposing
    ( Model
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
    , removePID
    , updateDocumentTitle
    , updateInstance
    )

import Dict
import Set
import System.Internal.PID exposing (PID(..), equals, system, toInt)


{-| The System Model
-}
type Model address actorName actorModel
    = Model (ModelRecord address actorName actorModel)


{-| Initialize a new System Model
-}
init : Model address actorName actorModel
init =
    Model
        { instances = Dict.empty
        , children = Dict.empty
        , addresses = []
        , lastPID = system
        , views = []
        , documentTitle = ""
        }


{-| Retrieve instance with given PID from the Model
-}
getInstance :
    PID
    -> Model address actorName actorModel
    -> Maybe ( actorName, actorModel )
getInstance pid (Model { instances }) =
    Dict.get (toInt pid) instances
        |> Maybe.map
            (\( _, actorName, actorModel ) ->
                ( actorName, actorModel )
            )


{-| Retrieve all instance PIDs currently active
-}
getInstances :
    Model address actorName actorModel
    -> List PID
getInstances (Model { instances }) =
    Dict.toList instances
        |> List.map (\( _, ( pid, _, _ ) ) -> pid)


{-| Retrieve possible children of given PID
-}
getChildren :
    PID
    -> Model address actorName actorModel
    -> Maybe (List PID)
getChildren pid (Model { children }) =
    Dict.get (toInt pid) children


{-| Retrieve views
-}
getViews :
    Model address actorName actorModel
    -> List PID
getViews (Model { views }) =
    views


{-| Retrieve the document title
-}
getDocumentTitle :
    Model address actorName actorModel
    -> String
getDocumentTitle (Model { documentTitle }) =
    documentTitle


{-| Retrieve an Address and it's PIDs
-}
getAddress :
    address
    -> Model address actorName actorModel
    -> ( address, List PID )
getAddress address (Model modelRecord) =
    ( address, List.map Tuple.second modelRecord.addresses )


{-| Get a new PID and Model based on the current Model
-}
getNewPID :
    Maybe PID
    -> Model address actorName actorModel
    -> ( PID, Model address actorName actorModel )
getNewPID maybeSpawendBy (Model ({ lastPID, children } as modelRecord)) =
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
    , Model
        { modelRecord
            | lastPID = pid
            , children = updatedChildren
        }
    )



-- Update


updateInstance :
    Model address actorName actorModel
    -> PID
    -> actorName
    -> actorModel
    -> Model address actorName actorModel
updateInstance (Model modelRecord) pid actorName actorModel =
    Model
        { modelRecord
            | instances =
                Dict.insert
                    (toInt pid)
                    ( pid, actorName, actorModel )
                    modelRecord.instances
        }


updateDocumentTitle :
    String
    -> Model address actorName actorModel
    -> Model address actorName actorModel
updateDocumentTitle documentTitle (Model modelRecord) =
    Model { modelRecord | documentTitle = documentTitle }



-- Add


addView :
    PID
    -> Model address actorName actorModel
    -> Model address actorName actorModel
addView pid (Model modelRecord) =
    Model { modelRecord | views = pid :: modelRecord.views }


addAddress :
    address
    -> PID
    -> Model address actorName actorModel
    -> Model address actorName actorModel
addAddress address pid (Model modelRecord) =
    Model { modelRecord | addresses = ( address, pid ) :: modelRecord.addresses }



-- Remove


removePID :
    PID
    -> Model address actorName actorModel
    -> Model address actorName actorModel
removePID pid (Model modelRecord) =
    let
        pidId =
            toInt pid
    in
    Model <|
        { modelRecord
            | instances = Dict.remove pidId modelRecord.instances
            , children = Dict.remove pidId modelRecord.children
            , views = List.filter (not << equals pid) modelRecord.views
            , addresses = List.filter (not << equals pid << Tuple.second) modelRecord.addresses
        }



-- Helpers


foldlInstances :
    ({ actorName : actorName, actorModel : actorModel, pid : PID } -> x -> x)
    -> x
    -> Model address actorName actorModel
    -> x
foldlInstances f initial (Model { instances }) =
    Dict.foldl
        (\_ ( pid, actorName, actorModel ) x ->
            f
                { pid = pid
                , actorName = actorName
                , actorModel = actorModel
                }
                x
        )
        initial
        instances



-- Types


type alias ModelRecord address actorName actorModel =
    { instances : Dict.Dict Int ( PID, actorName, actorModel )
    , children : Dict.Dict Int (List PID)
    , addresses : List ( address, PID )
    , lastPID : PID
    , views : List PID
    , documentTitle : String
    }
