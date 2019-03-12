--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Witter.API where 
    
--------------------------------------------------------------------------------

import Data.Text

import Servant 
import Servant.Docs

import Witter.Follower
import Witter.User
import Witter.Weet
import Witter.Util

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------

instance ToCapture (Capture "weet" Int) where
    toCapture _ = DocCapture "weet" "The unique ID of the weet."

instance ToCapture (Capture "user" Int) where
    toCapture _ = DocCapture "user" "The unique ID of the user."

instance ToCapture (Capture "beforeDate" Date) where
    toCapture _ = DocCapture "beforeDate" "Return data before this date."

instance ToCapture (Capture "onDate" Date) where
    toCapture _ = DocCapture "onDate" "Return data on this date."

instance ToSample Text where 
    toSamples _ = [("message","I love MGP #notacult")]

--------------------------------------------------------------------------------
