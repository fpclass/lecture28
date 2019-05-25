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

handle404 :: Maybe a -> Handler a
handle404 (Just x) = return x
handle404 Nothing  = throwError err404

witterServer :: WeetStore -> Server WitterApi 
witterServer ws = return (getWeets ws) 
             :<|> (\wid -> handle404 (getWeet wid ws))

witterApp :: WeetStore -> Application 
witterApp ws = serve witterApi (witterServer ws)

-- | `main` is the main entry point of this application.
main :: IO ()
main = do 
    fs <- loadFollowerStore "followers.csv"
    us <- loadUserStore "names.csv"
    ws <- loadWeetStore "weets.csv"
    
    run 8081 (witterApp ws)

--------------------------------------------------------------------------------
