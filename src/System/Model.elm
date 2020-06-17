module System.Model exposing (SystemModel)

{-|

@docs SystemModel

-}

import System.Internal.Model as Model


{-| An alias of the Model used by any application you start unsing this package.
-}
type alias SystemModel addresses actors appModel =
    Model.SystemModel addresses actors appModel
