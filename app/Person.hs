{-# LANGUAGE DeriveGeneric #-}

module Person where

import Data.Aeson
import Data.Text
import GHC.Generics

data Person = Person {
    name :: Text,
    age  :: Int
} deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person