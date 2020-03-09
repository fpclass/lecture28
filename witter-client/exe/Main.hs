--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with type-level programming                                   --
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
witterJS = jsForAPI witterApi $ jqueryWith defCommonGeneratorOptions {
    urlPrefix = "http://localhost:8081"
}

main :: IO ()
main = do 
    T.writeFile "wwwroot/js/client.js" witterJS
    writeFile "docs.md" (markdown witterDocs)

--------------------------------------------------------------------------------
