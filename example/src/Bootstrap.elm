module Bootstrap exposing (AppModel(..), bootstrap)

import ActorName exposing (ActorName(..))
import Actors.Counter as Counter exposing (Model, actor)
import Actors.Counters as Counters exposing (Model, actor)
import System.Actor exposing (toSystemActor)


type AppModel
    = CountersModel Counters.Model
    | CounterModel Counter.Model


bootstrap =
    { apply = apply
    , factory = factory
    }


actors =
    { counters = Counters.actor CountersModel
    , counter = Counter.actor CounterModel
    }


factory actorName =
    case actorName of
        Counters ->
            actors.counters.init

        Counter ->
            actors.counter.init


apply model =
    case model of
        CountersModel m ->
            toSystemActor actors.counters m

        CounterModel m ->
            toSystemActor actors.counter m
