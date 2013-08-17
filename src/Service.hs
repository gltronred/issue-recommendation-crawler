{-# LANGUAGE OverloadedStrings #-}

import Types

import           Control.Exception        (SomeException)
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=), fromJSON, toJSON)
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (ResourceT, ($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import qualified Data.Set as S
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, requestBody,
                                           responseLBS)
import           Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app

app :: Application
app req = handle invalidJson $ do
    value <- requestBody req $$ sinkParser json
    newValue <- liftIO $ modValue value
    return $ responseLBS
        status200
        [("Content-Type", "application/json")]
        $ encode newValue

invalidJson :: SomeException -> ResourceT IO Response
invalidJson ex = return $ responseLBS
    status400
    [("Content-Type", "application/json")]
    $ encode $ object
        [ ("message" .= show ex)
        ]

-- Application-specific logic would go here.
modValue :: UserIn -> IO User
modValue input = do
  putStrLn $ "Get user info for " ++ show input
  return $ User (S.fromList ["haskell", "bash", "unknown"]) (S.fromList ["aeson", "bytestring"])