module System.Internal.ModelTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import System.Debug exposing (pidToInt)
import System.Internal.Model as Model
import System.Internal.PID as PID
import Test exposing (..)


suite : Test
suite =
    describe "Model"
        [ describe "Model - PID creation"
            [ test "Get a new PID from a new Model."
                (\_ ->
                    Expect.equal
                        (Model.getNewPID Nothing Model.init
                            |> Tuple.first
                            |> pidToInt
                        )
                        2
                )
            , test "Get a new PID from a Model that already had a PID created before/"
                (\_ ->
                    Expect.equal
                        (Model.getNewPID Nothing Model.init
                            |> Tuple.second
                            |> Model.getNewPID Nothing
                            |> Tuple.first
                            |> pidToInt
                        )
                        3
                )
            ]
        , describe "Model - Instances"
            [ test "Add a new Instance and look it up"
                (\_ ->
                    let
                        ( newPid, modelWithNewPid ) =
                            Model.getNewPID Nothing Model.init
                    in
                    Expect.equal
                        (Model.updateInstance newPid "actorName" "actorModel" modelWithNewPid
                            |> Model.getInstance newPid
                        )
                        (Just ( "actorName", "actorModel" ))
                )
            , test "Add a new Instance and retrieve all Instances"
                (\_ ->
                    Expect.equal
                        (Model.getNewPID Nothing Model.init
                            |> (\( newPid, model ) -> Model.updateInstance newPid "actorName" "actorModel" model)
                            |> Model.getInstances
                            |> List.map pidToInt
                        )
                        [ 2 ]
                )
            ]
        , describe "Model - Children"
            [ test "Add a new Instance that was spawned by a previously spawned Instance"
                (\_ ->
                    let
                        ( pidA, modelWithPidA ) =
                            Model.getNewPID Nothing Model.init

                        modelWithInstanceA =
                            Model.updateInstance pidA "actorNameA" "actorModelA" modelWithPidA

                        ( pidB, modelWithPidAandB ) =
                            Model.getNewPID (Just pidA) modelWithInstanceA

                        modelWithInstanceAandB =
                            Model.updateInstance pidB "actorNameB" "actorModelB" modelWithPidAandB
                    in
                    Expect.equal
                        (Model.getChildren pidA modelWithInstanceAandB
                            |> Maybe.map (List.map pidToInt)
                        )
                        (Just [ 3 ])
                )
            ]
        , describe "Model - Views"
            [ test "Add a PID to the system views"
                (\_ ->
                    let
                        ( pidA, modelWithPidA ) =
                            Model.getNewPID Nothing Model.init

                        modelWithPidAasAView =
                            Model.addView pidA modelWithPidA
                    in
                    Expect.equal
                        (Model.getViews modelWithPidAasAView
                            |> List.map pidToInt
                        )
                        [ 2 ]
                )
            ]
        , describe "Model - Address"
            [ test "Add a PID to an Address"
                (\_ ->
                    let
                        address =
                            "JanJuc"

                        ( pidA, modelWithPidA ) =
                            Model.getNewPID Nothing Model.init

                        modelWithPidAPopulatedAddress =
                            Model.addAddress address pidA modelWithPidA
                    in
                    Expect.equal
                        (Model.getAddress address modelWithPidAPopulatedAddress
                            |> Tuple.mapSecond (List.map pidToInt)
                        )
                        ( address, [ 2 ] )
                )
            , test "Add multiple PIDs to an Address"
                (\_ ->
                    let
                        address =
                            "JanJuc"

                        ( pidA, modelWithPidA ) =
                            Model.getNewPID Nothing Model.init

                        modelWithPidAPopulatedAddress =
                            Model.addAddress address pidA modelWithPidA

                        ( pidB, modelWithPidB ) =
                            Model.getNewPID Nothing modelWithPidAPopulatedAddress

                        modelWithPidAandBPopulatedAddress =
                            Model.addAddress address pidB modelWithPidB
                    in
                    Expect.equal
                        (Model.getAddress address modelWithPidAandBPopulatedAddress
                            |> Tuple.mapSecond (List.map pidToInt)
                        )
                        ( address, [ 3, 2 ] )
                )
            ]
        , describe "Model - DocumentTitle"
            [ test "Change the documentTitle"
                (\_ ->
                    Expect.equal
                        (Model.updateDocumentTitle "new title" Model.init
                            |> Model.getDocumentTitle
                        )
                        "new title"
                )
            ]
        , describe "Model - PID Removal"
            [ test "Add a PID to everything and then remove it!"
                (\_ ->
                    let
                        ( pidA, modelWithPidA ) =
                            Model.getNewPID Nothing Model.init

                        modelWithPidBeingInUseEverywhere =
                            Model.updateInstance pidA "actorName" "actorModel" modelWithPidA
                                |> Model.addAddress "address" pidA
                                |> Model.addView pidA

                        lengthOfCombinedLists model =
                            { instances = List.length <| Model.getInstances model
                            , addresses = List.length <| Tuple.second <| Model.getAddress "address" model
                            , views = List.length <| Model.getViews model
                            , children = List.length <| Maybe.withDefault [] <| Model.getChildren PID.system model
                            }
                    in
                    Expect.equal
                        ( lengthOfCombinedLists modelWithPidBeingInUseEverywhere
                        , lengthOfCombinedLists (Model.removePID pidA modelWithPidBeingInUseEverywhere)
                        )
                        ( { instances = 1
                          , addresses = 1
                          , views = 1
                          , children = 1
                          }
                        , { instances = 0
                          , addresses = 0
                          , views = 0
                          , children = 0
                          }
                        )
                )
            ]
        ]
