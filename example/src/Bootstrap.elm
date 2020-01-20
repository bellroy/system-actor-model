module Bootstrap exposing (AppModel(..), bootstrap)

import ActorName exposing (ActorName(..))
import Actors.Counter as Counter exposing (Model, actor)
import Actors.Counters as Counters exposing (Model, actor)
import Actors.Snackbar as Snackbar exposing (Model, actor)
import Actors.Templating as Templating exposing (Model, actor)
import System.Actor exposing (toSystemActor)


type AppModel
    = CountersModel Counters.Model
    | CounterModel Counter.Model
    | SnackbarModel Snackbar.Model
    | TemplatingModel Templating.Model


bootstrap =
    { apply = apply
    , factory = factory
    }


actors =
    { counters = Counters.actor CountersModel
    , counter = Counter.actor CounterModel
    , snackbar = Snackbar.actor SnackbarModel
    , templating = Templating.actor TemplatingModel
    }


factory actorName =
    case actorName of
        Counters ->
            actors.counters.init

        Counter ->
            actors.counter.init

        Snackbar ->
            actors.snackbar.init

        Templating ->
            actors.templating.init


apply model =
    case model of
        CountersModel m ->
            toSystemActor actors.counters m

        CounterModel m ->
            toSystemActor actors.counter m

        SnackbarModel m ->
            toSystemActor actors.snackbar m

        TemplatingModel m ->
            toSystemActor actors.templating m
