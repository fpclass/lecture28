--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Witter.FollowerStore (
    FollowerStore(..),
    getFollowers,
    getFollowing,
    loadFollowerStore
) where 
    
--------------------------------------------------------------------------------

import Data.Csv
import Data.IntMap as IM
import Data.HashMap.Strict as HM
import Data.Maybe
import Data.Set as S
import Data.Vector as V

import Witter.Follower
import Witter.Util

--------------------------------------------------------------------------------

-- | Represents various indices for `Follower` data.
data FollowerStore = FollowerStore {
    followerStoreFollowers :: IM.IntMap (HM.HashMap Int Follower),
    followerStoreFollowing :: IM.IntMap (HM.HashMap Int Follower)
}

--------------------------------------------------------------------------------

buildFollowersIndex :: Vector Follower -> IM.IntMap (HM.HashMap Int Follower) 
buildFollowersIndex = IM.map convert . V.foldr add IM.empty 
    where add f = IM.alter (appendNonUniqueIndex f) (followerUserID f)
          convert xs = HM.fromList [(followerFollowerID x,x) | x <- xs]

buildFollowingIndex :: Vector Follower -> IM.IntMap (HM.HashMap Int Follower) 
buildFollowingIndex = IM.map convert . V.foldr add IM.empty 
    where add f = IM.alter (appendNonUniqueIndex f) (followerFollowerID f)
          convert xs = HM.fromList [(followerUserID x,x) | x <- xs]


--------------------------------------------------------------------------------

getFollowers :: Int -> FollowerStore -> Maybe [Follower]
getFollowers key = fmap HM.elems . IM.lookup key . followerStoreFollowers

getFollowing :: Int -> FollowerStore -> Maybe [Follower]
getFollowing key = fmap HM.elems . IM.lookup key . followerStoreFollowing

isAFollower :: Int -> Int -> FollowerStore -> Bool 
isAFollower key follows fs = fromMaybe False $ do 
    xs <- IM.lookup key (followerStoreFollowing fs)
    HM.lookup follows xs
    pure True
    
getNumFollowers :: Int -> FollowerStore -> Maybe Int 
getNumFollowers key = fmap HM.size . IM.lookup key . followerStoreFollowers

getMutualFollowers :: Int -> Int -> FollowerStore -> Maybe [Int]
getMutualFollowers key1 key2 fs = do 
    xs <- S.fromList . HM.keys <$> IM.lookup key1 (followerStoreFollowers fs)
    ys <- S.fromList . HM.keys <$> IM.lookup key2 (followerStoreFollowers fs)
    pure $ S.toList (xs `S.intersection` ys)

getMutualFollows :: Int -> Int -> FollowerStore -> Maybe [Int]
getMutualFollows key1 key2 fs = do 
    xs <- S.fromList . HM.keys <$> IM.lookup key1 (followerStoreFollowing fs)
    ys <- S.fromList . HM.keys <$> IM.lookup key2 (followerStoreFollowing fs)
    pure $ S.toList (xs `S.intersection` ys)

loadFollowerStore :: FilePath -> IO FollowerStore 
loadFollowerStore fp = do 
    result <- decodeFile NoHeader fp
    case result of 
        Left err -> fail err 
        Right followers -> return $ FollowerStore {
            followerStoreFollowers = buildFollowersIndex followers,
            followerStoreFollowing = buildFollowingIndex followers
        }

--------------------------------------------------------------------------------