--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Main (main) where 
    
--------------------------------------------------------------------------------

import Control.Monad.IO.Class

import Data.Time.Clock

import Network.Wai.Handler.Warp

import GHC.Conc

import Servant 
import Servant.Server

import Witter.API 
import Witter.Util
import Witter.Follower
import Witter.FollowerStore
import Witter.User
import Witter.UserStore
import Witter.Weet
import Witter.WeetStore

--------------------------------------------------------------------------------

-- | `handle404` @maybeValue@ returns a HTTP 404 error if @maybeValue@ is 
-- `Nothing` or the inner value if it is `Just`. 
handle404 :: Maybe a -> Handler a 
handle404 (Just x) = pure x 
handle404 Nothing  = throwError err404

weetsHandler :: WeetStore -> Handler [Weet]
weetsHandler ws = return (getWeets ws)

userHandler :: UserStore -> Int -> Handler User 
userHandler us user = handle404 (getUser user us)

followersHandler :: FollowerStore -> Int -> Handler [Follower]
followersHandler fs user = handle404 (getFollowers user fs)

followingHandler :: FollowerStore -> Int -> Handler [Follower]
followingHandler fs user = handle404 (getFollowing user fs)

witterServer :: UserStore -> TVar WeetStore -> FollowerStore -> Server WitterApi
witterServer us wvar fs = 
        (liftIO (atomically (readTVar wvar)) >>= weetsHandler)
   :<|> (\key -> liftIO (atomically (readTVar wvar)) >>= \ws -> handle404 (getWeet key ws))
   :<|> (\date -> liftIO (atomically (readTVar wvar)) >>= \ws -> return (getWeetsBefore date ws))
   :<|> (\date -> liftIO (atomically (readTVar wvar)) >>= \ws -> return (getWeetsOn date ws))
   :<|> userHandler us
   :<|> (\user -> liftIO (atomically (readTVar wvar)) >>= \ws -> handle404 (getWeetsByUser user ws))
   :<|> followersHandler fs
   :<|> followingHandler fs
   :<|> return (getUsers us)
   :<|> (\date -> return (getUsersJoinedBefore date us))
   :<|> (\body -> do 
            time <- liftIO getCurrentTime
            let weet = Weet 0 5 body time
            liftIO $ atomically $ do 
                ws <- readTVar wvar
                writeTVar wvar (addWeet weet ws)
            return weet)

-- | `witterApp` @userStore weetStore followerStore@ constructs a WAI 
-- `Application` for Witter using @userStore@, @weetStore@, and @followerStore@
-- as data sources.
witterApp :: UserStore -> TVar WeetStore -> FollowerStore -> Application
witterApp us ws fs = serve witterApi (witterServer us ws fs)

-- | `main` is the main entry point of this application.
main :: IO ()
main = do 
    us <- loadUserStore "names.csv"
    ws <- loadWeetStore "weets.csv"
    fs <- loadFollowerStore "followers.csv"
    wvar <- newTVarIO ws
    run 8081 (witterApp us wvar fs)

--------------------------------------------------------------------------------
