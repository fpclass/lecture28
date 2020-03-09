--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with type-level programming                                   --
--------------------------------------------------------------------------------

module Witter.WeetStore (
    WeetStore(..),
    addWeet,
    getWeets,
    getWeet,
    getWeetsByUser,
    getWeetsOn,
    getWeetsBefore,
    loadWeetStore
) where

--------------------------------------------------------------------------------

import Data.Csv
import qualified Data.IntMap as IM
import Data.Map as M
import Data.Maybe
import Data.HashMap.Strict as HM
import Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import qualified Data.Vector as V

import Witter.Util
import Witter.Weet

--------------------------------------------------------------------------------

-- | Represents a database of weets with several indicies.
data WeetStore = WeetStore {
    weetStoreIndex     :: IM.IntMap Weet,
    weetStoreUserIndex :: IM.IntMap [Weet],
    weetStoreTimeIndex :: M.Map Day [Weet]
}

--------------------------------------------------------------------------------

buildIndex :: V.Vector Weet -> IM.IntMap Weet 
buildIndex = V.foldr (\w m -> IM.insert (weetID w) w m) IM.empty

buildUserIndex :: V.Vector Weet -> IM.IntMap [Weet] 
buildUserIndex = V.foldr add IM.empty
    where add w = IM.alter (appendNonUniqueIndex w) (weetUser w)

buildTimeIndex :: V.Vector Weet -> M.Map Day [Weet] 
buildTimeIndex = V.foldr add M.empty 
    where add w = M.alter (appendNonUniqueIndex w) (utctDay $ weetWeeted w)

--------------------------------------------------------------------------------

-- | `addWeet` @weet weetStore@ adds @weet@ to @weetStore@.
addWeet :: Weet -> WeetStore -> WeetStore 
addWeet w ws@WeetStore{..} = ws {
    weetStoreIndex = 
        IM.insert (weetID w) w weetStoreIndex,
    weetStoreUserIndex = 
        IM.alter (appendNonUniqueIndex w) (weetUser w) weetStoreUserIndex,
    weetStoreTimeIndex = 
        M.alter (appendNonUniqueIndex w) (utctDay $ weetWeeted w) weetStoreTimeIndex
}

-- | `getWeet` @key weetStore@ retrieves the `Weet` value whose key is @key@ 
-- from @weetStore@.
getWeet :: Int -> WeetStore -> Maybe Weet 
getWeet key = IM.lookup key . weetStoreIndex

-- | `getWeets` @weetStore@ retrieves all `Weet` values from @weetStore@.
getWeets :: WeetStore -> [Weet]
getWeets = Prelude.concat . M.elems . weetStoreTimeIndex 

-- | `getWeetsByUser` @user weetStore@ retrieves all `Weet` values which 
-- were created by @user@. This function will return `Nothing` if the user 
-- does not exist.
getWeetsByUser :: Int -> WeetStore -> Maybe [Weet]
getWeetsByUser key = IM.lookup key . weetStoreUserIndex

-- | `getWeetsOn` @date weetStore@ retrieves all `Weet` values from @weetStore@ 
-- that were created on @date@.
getWeetsOn :: Date -> WeetStore -> [Weet]
getWeetsOn date = fromMaybe [] . M.lookup day . weetStoreTimeIndex
    where day = utctDay $ getDate date

-- | `getWeetsBefore` @date weetStore@ retrieves all `Weet` values from
-- @weetStore@ that were created before @date@.
getWeetsBefore :: Date -> WeetStore -> [Weet]
getWeetsBefore date = 
    Prelude.concat . M.elems . takeWhileAntitone (< day) . weetStoreTimeIndex
    where day = utctDay (getDate date) 

loadWeetStore :: FilePath -> IO WeetStore 
loadWeetStore fp = do 
    result <- decodeFile NoHeader fp
    case result of 
        Left err -> fail err 
        Right weets -> return $ WeetStore {
            weetStoreIndex = buildIndex weets,
            weetStoreUserIndex = buildUserIndex weets,
            weetStoreTimeIndex = buildTimeIndex weets
        }

--------------------------------------------------------------------------------
