{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module Models.Fragment
    ( Fragment
    , fragments
    ) where

import Data.Aeson
import Data.Aeson.TH
import GHC.Generics (Generic)

data Fragment = Fragment
  { fragmentId :: Int
  , editionId  :: Int
  , text       :: String
  } deriving (Eq, Show, Generic)

$(deriveJSON defaultOptions ''Fragment)

fragments :: [Fragment]
fragments = [Fragment 1 1 "It's a start"]
