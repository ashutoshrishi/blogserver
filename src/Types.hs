module Types
    ( -- * Server monad
        Server, ServerState(..)
    , Environment(..), getEnvironment, getOptions
    ) where


import Control.Monad.Trans.State.Lazy (StateT)
import Network.Wai.Handler.Warp (defaultSettings)
import System.Environment (lookupEnv)
import Web.Scotty (Options(..))


-- |State monad holding the configuration of a running server.
data ServerState = ServerState
    { environment :: Environment }

-- |Hold the server state over the standard IO monad.
type Server = StateT ServerState IO


-- |Represents different environment variables that can be set.
data Environment = Development | Production
    deriving (Show, Eq, Read)


getEnvironment :: IO Environment
getEnvironment = maybe Development read <$> lookupEnv "SCOTTY_ENV"




-- | Derive Scotty options from the server configuration
getOptions :: Environment -> IO Options
getOptions e = do
    let v = case e of
                Development -> 1
                Production -> 0
    -- For now we are going with default Warp settings
    return $ Options v defaultSettings
