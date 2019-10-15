module System.Internal.Message exposing
    ( Message(..), Control(..), toString
    , LogMessage(..), Severity(..), logMessageToString, severityToString, logMessageToMeta
    )

{-|


# Message

@docs Message, Control, toString


# Logging

@docs LogMessage, Severity, logMessageToString, severityToString, logMessageToMeta

-}

import Json.Decode exposing (Value)
import System.Internal.PID exposing (PID(..), toInt)
import Time exposing (Month(..), Posix, toDay, toHour, toMinute, toMonth, toSecond, toYear, utc)


{-| The System Message Type
-}
type Message address actorName appMsg
    = NoOp
    | ActorMsg appMsg
    | UnmappedMsg appMsg
    | Control (Control address actorName (Message address actorName appMsg))
    | Context PID (Message address actorName appMsg)
    | UpdateDocumentTitle String
    | Log (LogMessage address actorName appMsg)


{-| The Ctrl Messages
-}
type Control address actorName message
    = Batch (List message)
    | Command (Cmd message)
    | SendToPID PID message
    | SendToAddress address message
    | Spawn actorName (PID -> message)
    | SpawnWithFlags Value actorName (PID -> message)
    | AddView PID
    | PopulateAddress address PID
    | Kill PID


toString : Message address actorName appMsg -> Maybe String
toString msg =
    case msg of
        NoOp ->
            Nothing

        ActorMsg _ ->
            Just "Message for Actor"

        UnmappedMsg _ ->
            Just "Unmapped Message"

        Control control ->
            controlToString control
                |> Maybe.map (\a -> "Control " ++ a)

        Context _ _ ->
            Nothing

        UpdateDocumentTitle a ->
            Just <| "UpdateDocumentTitle " ++ a

        Log _ ->
            Nothing


controlToString : Control address actorName message -> Maybe String
controlToString control =
    case control of
        Batch _ ->
            Nothing

        Command _ ->
            Nothing

        SendToPID pid _ ->
            Just <| "SendToPID " ++ String.fromInt (toInt pid)

        SendToAddress _ _ ->
            Just <| "SendToAddress address"

        Spawn _ _ ->
            Just "Spawn"

        SpawnWithFlags _ _ _ ->
            Just "SpawnWithFlags"

        AddView pid ->
            Just <| "AddView " ++ String.fromInt (toInt pid)

        PopulateAddress _ pid ->
            Just <| "PopulateAddress address " ++ String.fromInt (toInt pid)

        Kill pid ->
            Just <| "Kill " ++ String.fromInt (toInt pid)



-- Logging


type Severity
    = Emergency
    | Alert
    | Critical
    | Error
    | Warning
    | Notice
    | Informational
    | Debug


type LogMessage address actorName appMsg
    = LogMessage
        { posix : Maybe Posix
        , severity : Severity
        , pid : PID
        , message : Maybe (Message address actorName appMsg)
        , description : String
        }


logMessageToString :
    LogMessage address actorName appMsg
    -> String
logMessageToString (LogMessage { posix, severity, pid, description }) =
    List.filterMap identity
        [ Maybe.map toUtcString posix
        , Just <| severityToString severity
        , Just <| pidToString pid
        , Just description
        ]
        |> String.join " | "


logMessageToMeta :
    LogMessage address actorName appMsg
    ->
        { posix : Maybe Posix
        , severity : Severity
        , pid : PID
        , message : Maybe (Message address actorName appMsg)
        , description : String
        }
logMessageToMeta (LogMessage meta) =
    meta


pidToString : PID -> String
pidToString pid =
    case pid of
        System ->
            "system"

        PID { id, spawnedBy } ->
            case spawnedBy of
                System ->
                    String.fromInt id

                PID parent ->
                    String.fromInt id ++ "(" ++ String.fromInt parent.id ++ ")"


severityToString : Severity -> String
severityToString severity =
    case severity of
        Emergency ->
            "emergency"

        Alert ->
            "alert"

        Critical ->
            "critical"

        Error ->
            "error"

        Warning ->
            "warning"

        Notice ->
            "notice"

        Informational ->
            "informational"

        Debug ->
            "debug"


toUtcString : Time.Posix -> String
toUtcString time =
    let
        monthToString month =
            case month of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"
    in
    String.fromInt (toYear utc time)
        ++ "/"
        ++ monthToString (toMonth utc time)
        ++ "/"
        ++ String.padLeft 2 '0' (String.fromInt (toDay utc time))
        ++ " "
        ++ String.padLeft 2 '0' (String.fromInt (toHour utc time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (toMinute utc time))
        ++ ":"
        ++ String.padLeft 2 '0' (String.fromInt (toSecond utc time))
        ++ " (UTC)"
