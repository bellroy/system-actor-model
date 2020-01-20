module Components.Counter exposing (Model, MsgIn, MsgOut(..), component)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Json.Encode as Encode
import System.Component.Ui exposing (Ui)
import System.Event exposing (systemDefault)
import System.Process exposing (PID, pidToString)


{-| Our model will hold the current Count and the PID the System will assign to this component
-}
type Model
    = Counter PID Steps Count


type alias Count =
    Int


type alias Steps =
    Int


{-| Think of MsgIn as your Msg type in a classical Elm application
-}
type MsgIn
    = Increment
    | Decrement


{-| Think of MsgOut as Elm Cmds that you would like the System to run
-}
type MsgOut
    = LogCreated


{-| Setting up a component is much like setting up a Core Browser.element
-}
component : Ui Model MsgIn MsgOut
component =
    { init = init
    , update = update
    , view = view
    , subscriptions = always Sub.none
    , events = systemDefault
    }


{-| When a component get's initialized you will receive it's assigned PID from the System
-}
init :
    ( PID, Decode.Value )
    -> ( Model, List MsgOut, Cmd MsgIn )
init ( pid, flags ) =
    let
        maybeCount =
            Decode.decodeValue Decode.int flags
                |> Result.toMaybe

        maybeValue =
            Decode.decodeValue (Decode.at [ "attributes", "value" ] Decode.string) flags
                |> Result.toMaybe
                |> Maybe.andThen String.toInt

        maybeSteps =
            Decode.decodeValue (Decode.at [ "attributes", "steps" ] Decode.string) flags
                |> Result.toMaybe
                |> Maybe.andThen String.toInt

        value =
            case ( maybeCount, maybeValue ) of
                ( Just _, Just a ) ->
                    a

                ( Just a, Nothing ) ->
                    a

                ( Nothing, Just a ) ->
                    a

                _ ->
                    0

        steps =
            Maybe.withDefault 1 maybeSteps
    in
    ( Counter pid steps value
    , [ LogCreated ]
    , Cmd.none
    )


update :
    MsgIn
    -> Model
    -> ( Model, List MsgOut, Cmd MsgIn )
update msgIn (Counter pid steps count) =
    case msgIn of
        Increment ->
            ( Counter pid steps (count + steps)
            , []
            , Cmd.none
            )

        Decrement ->
            ( Counter pid steps (count - steps)
            , []
            , Cmd.none
            )


view :
    Model
    -> Html MsgIn
view (Counter pid steps count) =
    tr []
        [ td [] [ text (pidToString pid) ]
        , td []
            [ strong [] [ text (String.fromInt count) ]
            ]
        , td []
            [ div [ class "btn-group" ]
                [ button [ class "btn btn-primary", onClick Decrement ] [ text "-" ]
                , button [ class "btn btn-primary", onClick Increment ] [ text "+" ]
                ]
            ]
        ]
