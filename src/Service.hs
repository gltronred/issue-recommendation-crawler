{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ImplicitParams #-}

import Types
import UserInfo

import           Control.Exception        (SomeException(..), Exception(..))
import           Control.Exception.Lifted (handle)
import           Control.Monad.IO.Class   (liftIO)
import           Data.Aeson               (Value, encode, object, (.=), fromJSON, toJSON, Result(..))
import           Data.Aeson.Parser        (json)
import           Data.ByteString          (ByteString)
import           Data.Conduit             (ResourceT, ($$))
import           Data.Conduit.Attoparsec  (sinkParser)
import qualified Data.Set as S
import           Data.Typeable
import           Network.HTTP.Types       (status200, status400)
import           Network.Wai              (Application, Response, requestBody,
                                           responseLBS)
import           Network.Wai.Handler.Warp (run)

data JSONParseException = JSONParseException deriving (Show, Typeable)

instance Exception JSONParseException

main :: IO ()
main = run 8080 app

app :: Application
app req = handle invalidJson $ do
    value <- requestBody req $$ sinkParser json
    let val' = fromJSON value :: Result UserIn
    case val' of
      Error err -> do
        liftIO $ putStrLn $ "Error in parsing JSON: "++err
        invalidJson $ SomeException JSONParseException
      Success res -> do
        newValue <- liftIO $ modValue res
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
modValue input = let
  ?verbose = 0
  in do
    putStrLn $ "Get user info for " ++ show input
    ui <- userInfo (userInName input) (userInToken input)
    return $ case ui of
      Nothing -> User S.empty S.empty
      Just u -> u

