--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

-- | Contains the representation of Weets as well as related type class 
-- instances for CSV and JSON.
module Witter.Weet where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Aeson
import Data.Csv hiding ((.=), (.:))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Format

import Servant.Docs

import Witter.Util

--------------------------------------------------------------------------------

-- | `extractHashtags` @message@ extracts a list of hashtags from @input@.
-- This is probably a bit hacky/inelegant; using a parser would be nicer and 
-- could e.g. avoid the empty hashtag more easily, but adding a parser library 
-- for this demo is overkill. 
extractHashtags :: Text -> [Text]
extractHashtags input = 
    let rs = T.dropWhile (/= '#') input 
    in if T.null rs then [] 
       else let (ht, rs') = T.span (/= ' ') rs 
            in ht : extractHashtags rs' 

--------------------------------------------------------------------------------

-- | Reprsents a single Weet.
data Weet = Weet {
    weetID      :: Int,
    weetUser    :: Int,
    weetMessage :: Text,
    weetWeeted  :: UTCTime
} deriving Show

instance FromRecord Weet where
    parseRecord v
        | length v == 4 = Weet <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3
        | otherwise     = mzero

instance ToJSON Weet where 
    toJSON Weet{..} = object 
        [ "id"       .= weetID
        , "user"     .= weetUser 
        , "message"  .= weetMessage 
        , "dateTime" .= weetWeeted
        ]

instance FromJSON Weet where 
    parseJSON = withObject "Weet" $ \v -> 
        Weet <$> v .: "id"
             <*> v .: "user"
             <*> v .: "message"
             <*> v .: "dateTime"

instance ToSample Weet where
    toSamples _ = [("weet",weet)]
        where dt = parseTimeOrError True defaultTimeLocale "%d-%M-%Y" "01-01-2019"
              weet = Weet { weetID = 0
                          , weetUser = 5
                          , weetMessage = "Hello there!"
                          , weetWeeted = dt
                          }

--------------------------------------------------------------------------------
