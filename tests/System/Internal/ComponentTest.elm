module System.Internal.ComponentTest exposing (suite)

import Expect
import Json.Encode as Encode
import System.Internal.Component as Component
import System.Internal.Message exposing (Control(..), Message(..))
import System.Internal.PID as PID exposing (PID)
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Component"
        [ Test.describe "Component - wrapInit"
            [ Test.test "Wrap init so that it will return an actorModel and System.Message"
                (\_ ->
                    Expect.equal
                        (Component.wrapInit
                            mockedArgs
                            mockedInit
                            ( PID.system, Encode.null )
                        )
                        ( "mockedInitModel", Context PID.system (Control (Batch [ NoOp ])) )
                )
            ]
        , Test.describe "Component - wrapUpdate"
            [ Test.test "Wrap update so that it will return an actorModel and System.Message"
                (\_ ->
                    Expect.equal
                        (Component.wrapUpdate
                            mockedArgs
                            mockedUpdate
                            "model"
                            NoOp
                            PID.system
                        )
                        ( "model", NoOp )
                )
            ]
        ]


mockedUpdate :
    String
    -> String
    -> ( String, List msgOut, Cmd msgIn )
mockedUpdate _ _ =
    ( "mockedUpdatedModel"
    , []
    , Cmd.none
    )


mockedInit :
    ( PID, a )
    -> ( String, List msgOut, Cmd msgIn )
mockedInit _ =
    ( "mockedInitModel"
    , []
    , Cmd.none
    )


mockedArgs :
    { wrapModel : String -> String
    , wrapMsg : String -> String
    , mapIn : String -> Maybe String
    , mapOut : PID -> msgOut -> Message address actorName String
    }
mockedArgs =
    { wrapModel = identity
    , wrapMsg = identity
    , mapIn = Just << identity
    , mapOut = \_ _ -> NoOp
    }
