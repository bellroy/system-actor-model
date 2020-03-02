module System.Internal.Message exposing
    ( Control(..), toString
    , LogMessage(..), Severity(..), logMessageToString, severityToString, logMessageToMeta
    , SystemMessage(..)
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
type SystemMessage applicationAddress applicationActorName applicationMessage
    = NoOp
    | ActorMsg applicationMessage
    | UnmappedMsg applicationMessage
    | Control (Control applicationAddress applicationActorName (SystemMessage applicationAddress applicationActorName applicationMessage))
    | Context PID (SystemMessage applicationAddress applicationActorName applicationMessage)
    | UpdateDocumentTitle String
    | Log (LogMessage applicationAddress applicationActorName applicationMessage)


{-| The Ctrl Messages
-}
type Control applicationAddress applicationActorName systemMessage
    = Batch (List systemMessage)
    | Command (Cmd systemMessage)
    | SendToPID PID systemMessage
    | SendToAddress applicationAddress systemMessage
    | SendToPidOnAddress PID applicationAddress systemMessage
    | SpawnWithFlags Value applicationActorName (PID -> systemMessage)
    | SpawnMultipleWithFlags (List ( applicationActorName, Value )) (List PID -> systemMessage)
    | AddView PID
    | PopulateAddress applicationAddress PID
    | RemoveFromView PID
    | RemoveFromAddress applicationAddress PID
    | Kill PID


toString :
    SystemMessage applicationAddress applicationActorName applicationMessage
    -> Maybe String
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
                |> Maybe.map ((++) "Control ")

        Context _ _ ->
            Nothing

        UpdateDocumentTitle a ->
            Just <| "UpdateDocumentTitle " ++ a

        Log _ ->
            Nothing


controlToString :
    Control applicationAddress applicationActorName message
    -> Maybe String
controlToString control =
    case control of
        Batch _ ->
            Nothing

        Command _ ->
            Nothing

        SendToPID pid _ ->
            Just <| "SendToPID " ++ String.fromInt (toInt pid)

        SendToAddress _ _ ->
            Just <| "SendToAddress applicationAddress"

        SendToPidOnAddress _ _ _ ->
            Just <| "SendToPidOnAddress pid applicationAddress"

        SpawnWithFlags _ _ _ ->
            Just "SpawnWithFlags"

        SpawnMultipleWithFlags _ _ ->
            Just "SpawnMultipleWithFlags"

        AddView pid ->
            Just <| "AddView " ++ String.fromInt (toInt pid)

        PopulateAddress _ pid ->
            Just <| "PopulateAddress applicationAddress " ++ String.fromInt (toInt pid)

        RemoveFromView pid ->
            Just <| "RemoveFromView " ++ String.fromInt (toInt pid)

        RemoveFromAddress _ pid ->
            Just <| "RemoveFromAddress applicationAddress " ++ String.fromInt (toInt pid)

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


type LogMessage applicationAddress applicationActorName applicationMessage
    = LogMessage
        { posix : Maybe Posix
        , severity : Severity
        , pid : PID
        , message : Maybe (SystemMessage applicationAddress applicationActorName applicationMessage)
        , description : String
        }


logMessageToString :
    LogMessage applicationAddress applicationActorName applicationMessage
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
    LogMessage applicationAddress applicationActorName applicationMessage
    ->
        { posix : Maybe Posix
        , severity : Severity
        , pid : PID
        , message : Maybe (SystemMessage applicationAddress applicationActorName applicationMessage)
        , description : String
        }
logMessageToMeta (LogMessage meta) =
    meta


pidToString :
    PID
    -> String
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


severityToString :
    Severity
    -> String
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


toUtcString :
    Time.Posix
    -> String
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
