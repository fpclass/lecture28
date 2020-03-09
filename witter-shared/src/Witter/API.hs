--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture: Fun with type-level programming                                   --
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

-- | The Witter API, as a type.
type WitterApi = "weets" :> Get '[JSON] [Weet]
            :<|> "weets" :> Capture "weet" Int :> Get '[JSON] Weet
            :<|> "weets" :> "before" :> Capture "beforeDate" Date :> Get '[JSON] [Weet]
            :<|> "weets" :> "on" :> Capture "onDate" Date :> Get '[JSON] [Weet]
            :<|> "user" :> Capture "user" Int :> Get '[JSON] User
            :<|> "user" :> Capture "user" Int :> "weets" :> Get '[JSON] [Weet]
            :<|> "user" :> Capture "user" Int :> "followers" :> Get '[JSON] [Follower]
            :<|> "user" :> Capture "user" Int :> "following" :> Get '[JSON] [Follower]
            :<|> "users" :> Get '[JSON] [User]
            :<|> "users" :> "before" :> Capture "beforeDate" Date :> Get '[JSON] [User]
            :<|> "weet" :> "post" :> ReqBody '[JSON] Text :> Post '[JSON] Weet

-- | A `Proxy` value for the `WitterApi` type.
witterApi :: Proxy WitterApi
witterApi = Proxy

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
