--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Witter.User where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson
import Data.Csv hiding ((.=), (.:))
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format

import Servant.Docs

import Witter.Util

--------------------------------------------------------------------------------

data User = User {
    userID      :: Int,
    userName    :: Text,
    userJoined  :: Date
} deriving Show

instance FromRecord User where
    parseRecord v
        | length v == 3 = User <$> v .! 2 <*> v .! 0 <*> v .! 1 
        | otherwise     = mzero

instance ToJSON User where 
    toJSON User{..} = object 
        [ "id"     .= userID
        , "name"   .= userName 
        , "joined" .= userJoined
        ]

instance FromJSON User where 
    parseJSON = withObject "User" $ \v -> 
        User <$> v .: "id"
             <*> v .: "name"
             <*> v .: "joined"

instance ToSample User where
    toSamples _ = [("user",user)]
     where dt = Date $ parseTimeOrError True defaultTimeLocale "%d-%M-%Y" "01-01-2019"
           user = User { userID     = 5
                       , userName   = "CS126 Student"
                       , userJoined = dt
                       }

--------------------------------------------------------------------------------
