--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Lecture 28: Fun with type-level programming                                --
--------------------------------------------------------------------------------

module Client where 
    
--------------------------------------------------------------------------------

import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)

import Servant.API
import Servant.Client

import Witter.API 

--------------------------------------------------------------------------------
    
-- | `witterEnv` @manager@ is a `ClientEnv` value for the Witter REST API,
-- configured for @localhost:8081@ where @manager@ is used for connections.
witterEnv :: Manager -> ClientEnv 
witterEnv manager = 
    ClientEnv manager (BaseUrl Http "localhost" 8081 "") Nothing
    
-- | `runWitterClient` @action@ runs @action@ against the Witter API using a 
-- new `Manager`.
runWitterClient :: ClientM a -> IO (Either ServantError a)
runWitterClient action = do 
    manager <- newManager defaultManagerSettings 
    runClientM action (witterEnv manager)
    
--------------------------------------------------------------------------------
    
getWeets :<|> 
    getWeet :<|>
    getWeetsBefore :<|>
    getWeetsOn :<|>
    getUser :<|>
    getUserWeets :<|>
    getFollowers :<|> 
    getFollowing :<|>
    getUsers :<|> 
    getUsersBefore :<|>
    postWeet = client witterApi

    
--------------------------------------------------------------------------------