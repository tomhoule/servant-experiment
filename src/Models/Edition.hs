{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Edition
    ( Edition
    , editions
    ) where

import GHC.Generics (Generic)
import Data.Aeson
import Data.Aeson.TH

data Edition = Edition
  { editionId :: Int
  , editor  :: String
  } deriving (Eq, Show, Generic)

editions :: [Edition]
editions = [ Edition 1 "Somebody" ]

$(deriveJSON defaultOptions ''Edition)
