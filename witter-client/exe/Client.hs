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

witterEnv :: Manager -> ClientEnv 
witterEnv manager = ClientEnv manager (BaseUrl Http "localhost" 8081 "") Nothing

runWitterClient :: ClientM a -> IO (Either ServantError a)
runWitterClient action = do 
    manager <- newManager defaultManagerSettings
    runClientM action (witterEnv manager)

--------------------------------------------------------------------------------
    
getWeets :<|> getWeet = client witterApi 
    
--------------------------------------------------------------------------------