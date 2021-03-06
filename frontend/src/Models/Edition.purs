-- File auto generated by purescript-bridge! --
module Models.Edition where

import Data.Lens (Lens', Prism', lens, prism')
import Data.Maybe (Maybe(..))
import Prim (Int, String)

import Prelude
import Data.Generic (class Generic)

newtype Edition =
    Edition {
      editionId :: Int
    , editor :: String
    }

derive instance genericEdition :: Generic Edition

--------------------------------------------------------------------------------
_Edition :: Prism' Edition { editionId :: Int, editor :: String}
_Edition = prism' Edition f
  where
    f (Edition r) = Just r


--------------------------------------------------------------------------------
