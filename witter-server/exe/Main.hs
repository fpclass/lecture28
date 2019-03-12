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


-- | `main` is the main entry point of this application.
main :: IO ()
main = return ()

--------------------------------------------------------------------------------
