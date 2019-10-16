module System.Platform exposing (Program)

{-|

@docs Program

-}

import System.Internal.Message exposing (Message)
import System.Internal.Model exposing (Model)


{-| This will be the type of your program when you create it using this package.

Checkout out the `element` and `application` functions in the System.Browser module to find out how to create a System.Program.

_A [Program](https://package.elm-lang.org/packages/elm/core/latest/Platform#Program) describes an Elm program! How does it react to input? Does it show anything on screen? Etc._

-}
type alias Program flags address actorName actorModel appMsg =
    Platform.Program
        --
        flags
        (Model address actorName actorModel)
        (Message address actorName appMsg)
