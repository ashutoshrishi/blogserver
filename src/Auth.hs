{-# LANGUAGE OverloadedStrings #-}
module Auth
    ( basicAuth ) where


import           Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson.Types as JS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base
import qualified Data.ByteString.Char8 as C
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as T
import           Network.HTTP.Types.Header (hAuthorization, RequestHeaders)
import           Network.Wai
import           Web.JWT
import           Web.Scotty (ActionM, raise)

basicAuth :: Request -> ActionM ()
basicAuth req = do
    (user, pass) <- maybe (raise "No Auth.") return $
        decodeAuthHeader (requestHeaders req)
    let jwt = makeJWT ((T.pack . C.unpack) user)
    liftIO $ print jwt
    liftIO $ print (user, pass)


decodeAuthHeader :: RequestHeaders -> Maybe (BS.ByteString, BS.ByteString)
decodeAuthHeader hds = do
    val <- List.lookup hAuthorization hds
    case Base.decode (BS.drop 6 val) of
        Left _ -> Nothing
        Right bs -> case C.split ':' bs of
                        [u,p] -> return (u,p)
                        _ -> Nothing


makeJWT :: T.Text -> JSON
makeJWT user =
    let cs = def {
            iss = stringOrURI "ashutoshr.com",
            sub = stringOrURI "login",
            unregisteredClaims = Map.fromList
                                 [ ("name", JS.String "John Doe")
                                 , ("user", JS.String user)
                                 ]
            }
        key = secret "temporary"
    in encodeSigned HS256 key cs
