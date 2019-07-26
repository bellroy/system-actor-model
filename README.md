# System Actor Model

First let me say that this is heavily inspired by the work on the ["Actor model in Elm"](https://package.elm-lang.org/packages/webbhuset/elm-actor-model/latest/)
package

This package has derived from the facts:
  - [webbhuset/elm-actor-model](https://package.elm-lang.org/packages/webbhuset/elm-actor-model/latest/) Is currently still in Alpha.
    large API changes are meant to be expected.
  - I personally wanted to understand the inner workings of said package better.
  - To be more of a minimal implementation

## The Actor Model

You can think of an Actor as an Elm application whit in an Elm application.

### Characteristics

  - An Actor get's spawned by the System or by another Actor.
  - An active Actor a.k.a a Process is identified by the System using a PID.
  - An Actor can talk to another Actor when it knows it PID.
  - An Actor can populate an Address
  - Different Actors can populate the same Address
  - An Actor can talk to an Address but there is no guarantee any or all residing Actors will respond.

## Documentation

The documentation is hosted on the [Elm package website](https://package.elm-lang.org/packages/tricycle/system-actor-model/latest).

## Demo

The example Application can be found in action [here](https://tricycle.github.io/system-actor-model/).

## Example Application

There is an example Application available in the example folder.

`cd example && elm reactor`
