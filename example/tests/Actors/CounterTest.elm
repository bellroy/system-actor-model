module Actors.CounterTest exposing (suite)

import Components.Counter as Counter
import Expect
import Html exposing (Html)
import Json.Encode as Encode
import Msg as Msg exposing (Msg)
import System.Actor exposing (Actor)
import System.Component.Ui exposing (toActor)
import System.Log exposing (info)
import System.Message as Message exposing (SystemMessage)
import System.Process exposing (PID)
import System.Sandbox as Sandbox exposing (Sandbox, SandboxActor)
import Test exposing (Test)
import Test.Html.Query as HtmlQuery
import Test.Html.Selector as HtmlSelector


 

actor : SandboxActor Counter.Model Counter.MsgIn (Html (SystemMessage () () Counter.MsgIn))
actor =
    toActor Counter.component
        { wrapModel = identity
        , wrapMsg = identity
        , mapIn = \a -> Just a
        , mapOut = \_ _ -> Message.noOperation
        }


toCounterFlags : { value : Int, steps : Int } -> Encode.Value
toCounterFlags { value, steps } =
    Encode.object
        [ ( "attributes"
          , Encode.object
                [ ( "value"
                  , String.fromInt value
                        |> Encode.string
                  )
                , ( "steps"
                  , String.fromInt steps
                        |> Encode.string
                  )
                ]
          )
        ]


suite : Test
suite =
    Test.describe "Counter"
        [ Test.describe "Spawn a new Counter"
            ([ { name = "Successfully start a counter without any flags"
               , useFlags = False
               , expect = { steps = 1, value = 0 }
               }
             , { name = "Successfully start a counter with step = 10 and value = 0"
               , useFlags = True
               , expect = { steps = 10, value = 0 }
               }
             , { name = "Successfully start a counter with step = 10 and value = 10"
               , useFlags = True
               , expect = { steps = 10, value = 10 }
               }
             ]
                |> List.map
                    (\{ name, useFlags, expect } ->
                        let
                            flags =
                                if useFlags then
                                    Just (toCounterFlags expect)

                                else
                                    Nothing
                        in
                        Test.describe name
                            [ Test.test (name ++ " expect model") <|
                                \_ ->
                                    Sandbox.spawn actor flags
                                        |> Sandbox.getModel
                                        |> (\(Counter.Counter _ steps value) ->
                                                Expect.equal ( steps, value ) ( expect.steps, expect.value )
                                           )
                            , Test.test (name ++ " expect view") <|
                                \_ ->
                                    Sandbox.spawn actor flags
                                        |> Sandbox.getView
                                        |> HtmlQuery.fromHtml
                                        |> HtmlQuery.find [ HtmlSelector.tag "strong" ]
                                        |> HtmlQuery.has
                                            [ expect.value
                                                |> String.fromInt
                                                |> HtmlSelector.text
                                            ]
                            ]
                    )
            )
        , Test.describe "Increment Counter"
            [ Test.test "Increment Counter 0 with 1" <|
                \_ ->
                    Sandbox.spawn actor Nothing
                        |> Sandbox.update Counter.Increment
                        |> Sandbox.getModel
                        |> (\(Counter.Counter _ _ value) ->
                                Expect.equal value 1
                           )
            , Test.test "Increment Counter 0 with 10" <|
                \_ ->
                    toCounterFlags { steps = 10, value = 0 }
                        |> Just
                        |> Sandbox.spawn actor
                        |> Sandbox.update Counter.Increment
                        |> Sandbox.getModel
                        |> (\(Counter.Counter _ _ value) ->
                                Expect.equal value 10
                           )
            ]
        , Test.describe "Decrement Counter"
            [ Test.test "Decrement Counter 0 with 1" <|
                \_ ->
                    Sandbox.spawn actor Nothing
                        |> Sandbox.update Counter.Decrement
                        |> Sandbox.getModel
                        |> (\(Counter.Counter _ _ value) ->
                                Expect.equal value -1
                           )
            , Test.test "Decrement Counter 40 with 10" <|
                \_ ->
                    toCounterFlags { steps = 10, value = 40 }
                        |> Just
                        |> Sandbox.spawn actor
                        |> Sandbox.update Counter.Decrement
                        |> Sandbox.getModel
                        |> (\(Counter.Counter _ _ value) ->
                                Expect.equal value 30
                           )
            ]
        ]
