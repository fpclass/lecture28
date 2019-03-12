--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Witter.UserStore (
    UserStore(..),
    addUser,
    getUser,
    getUsers,
    getUsersJoinedBefore,
    loadUserStore
) where 
    
--------------------------------------------------------------------------------

import Data.Csv
import Data.IntMap as IM
import Data.Map as M
import Data.Time.Clock
import Data.Vector as V

import Witter.User
import Witter.Util

--------------------------------------------------------------------------------

-- | Represents various indices for users in the system.
data UserStore = UserStore {
    userStoreIndex  :: IM.IntMap User,
    userStoreJoined :: M.Map Date [User]
}

--------------------------------------------------------------------------------

buildIndex :: Vector User -> IM.IntMap User
buildIndex = V.foldr add IM.empty 
    where add u = IM.insert (userID u) u

buildJoinedIndex :: Vector User -> M.Map Date [User] 
buildJoinedIndex = V.foldr add M.empty 
    where add u = M.alter (appendNonUniqueIndex u) (userJoined u)

--------------------------------------------------------------------------------

-- | `addUser` @user userStore@ adds @user@ to @userStore@.
addUser :: User -> UserStore -> UserStore
addUser user us@UserStore{..} = 
    us { userStoreIndex = IM.insert (userID user) user userStoreIndex 
       , userStoreJoined = M.alter 
            (appendNonUniqueIndex user) 
            (userJoined user) 
            userStoreJoined
       }

-- | `getUser` @id userStore@ tries to retrieve the user whose unique id is 
-- @id@ from @userStore@.
getUser :: Int -> UserStore -> Maybe User 
getUser key = IM.lookup key . userStoreIndex

-- | `getUsers` @userStore@ retrieves all users from @userStore@, sorted by 
-- the date they joined.
getUsers :: UserStore -> [User]
getUsers = Prelude.concat . M.elems . userStoreJoined 

-- | `getUsersJoinedBefore` @date userStore@ retrieves all users from 
-- @userStore@ who joined before @date@.
getUsersJoinedBefore :: Date -> UserStore -> [User]
getUsersJoinedBefore date = 
    Prelude.concat . M.elems . takeWhileAntitone (< date) . userStoreJoined

-- | `loadUserStore` @filePath@ loads a `UserStore` value from a CSV file 
-- located at @filePath@.
loadUserStore :: FilePath -> IO UserStore 
loadUserStore fp = do 
    result <- decodeFile NoHeader fp
    case result of 
        Left err -> fail err 
        Right users -> return $ UserStore {
            userStoreIndex  = buildIndex users, 
            userStoreJoined = buildJoinedIndex users
        }

--------------------------------------------------------------------------------