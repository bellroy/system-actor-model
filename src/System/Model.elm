module System.Model exposing (SystemModel)

{-| Model

@docs SystemModel

-}

import System.Internal.Model as Model


{-| The alias of the Internal System Message
-}
type alias SystemModel applicationAddress applicationActorName applicationModel =
    Model.SystemModel applicationAddress applicationActorName applicationModel
