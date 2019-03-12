--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Witter.Follower where 
    
--------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson
import Data.Csv hiding ((.=), (.:))
import Data.Time.Clock
import Data.Time.Format

import Servant.Docs

import Witter.Util

--------------------------------------------------------------------------------

-- | Represents a follower relationship.
data Follower = Follower {
    followerUserID     :: Int, -- ^ The ID of the user who is being followed. 
    followerFollowerID :: Int, -- ^ The ID of the user who is following.
    followerDateTime   :: UTCTime 
}

instance FromRecord Follower where
    parseRecord v
        | length v == 3 = Follower <$> v .! 0 <*> v .! 1 <*> v .! 2
        | otherwise     = mzero

instance ToJSON Follower where 
    toJSON Follower{..} = object 
        [ "id"       .= followerUserID
        , "follower" .= followerFollowerID 
        , "dateTime" .= followerDateTime
        ]

instance FromJSON Follower where 
    parseJSON = withObject "Follower" $ \v -> 
        Follower <$> v .: "id"
                 <*> v .: "follower"
                 <*> v .: "dateTime"

instance ToSample Follower where
    toSamples _ = [("follower",follower)]
     where dt = parseTimeOrError True defaultTimeLocale "%d-%M-%Y" "01-01-2019"
           follower = Follower { followerUserID     = 5
                               , followerFollowerID = 7
                               , followerDateTime   = dt
                               }

--------------------------------------------------------------------------------    