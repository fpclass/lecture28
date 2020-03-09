--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with type-level programming                                   --
--------------------------------------------------------------------------------

-- | This module contains various utility functions, types, and type class 
-- instances for other modules in the Witter application.
module Witter.Util where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson hiding (decode)
import Data.ByteString.Char8 as C8
import Data.ByteString.Lazy as L
import Data.Csv
import Data.Text as T
import Data.Time.Clock
import Data.Time.Format
import Data.Vector (Vector)

import System.IO

import Servant.API

--------------------------------------------------------------------------------

newtype Date = Date { getDate :: UTCTime }
    deriving (Eq, Ord, Show)

instance FromField Date where 
    parseField f =
        Date <$> parseTimeM True defaultTimeLocale "%d/%M/%Y" (C8.unpack f)

instance ToJSON Date where 
    toJSON = toJSON . getDate

instance FromJSON Date where 
    parseJSON v = Date <$> parseJSON v

instance FromHttpApiData Date where 
    parseUrlPiece text = 
        Date <$> parseTimeM True defaultTimeLocale "%d-%M-%Y" (T.unpack text)

instance ToHttpApiData Date where 
    toUrlPiece (Date date) = 
        T.pack $ formatTime defaultTimeLocale "%d-%M-%Y" date

instance FromField UTCTime where
    parseField f =
        parseTimeM True defaultTimeLocale "%d/%M/%Y %I:%M %p" (C8.unpack f)

--------------------------------------------------------------------------------

-- | `decodeFile` @hasHeader filePath@ decodes the CSV file at @filePath@.
decodeFile ::
    FromRecord a => HasHeader -> FilePath -> IO (Either String (Vector a))
decodeFile hh fp = decode hh <$> L.readFile fp

--------------------------------------------------------------------------------

-- | `appendNonUniqueIndex` @value maybeListOfValues@ is meant to be used 
-- with the @alter@ functions from e.g. `Data.Map` to add @value@ to a list
-- that represents a non-unique index.
appendNonUniqueIndex :: a -> Maybe [a] -> Maybe [a]
appendNonUniqueIndex u Nothing   = pure [u]
appendNonUniqueIndex u (Just us) = pure (u:us)

--------------------------------------------------------------------------------
