--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Main (main) where 
    
--------------------------------------------------------------------------------

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Servant.Docs
import Servant.JS

import System.IO

import Witter.API
import Client

--------------------------------------------------------------------------------

witterDocs :: API 
witterDocs = docs witterApi 

witterJS :: T.Text 
witterJS = jsForAPI witterApi jquery

-- | `main` is the main entry point of this application.
main :: IO ()
main = do 
    writeFile "docs.md" (markdown witterDocs)
    T.writeFile "client.js" witterJS

--------------------------------------------------------------------------------
