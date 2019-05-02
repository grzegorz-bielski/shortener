{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class    (liftIO)
import qualified Data.Aeson                as A
import qualified Data.ByteString.Char8     as BC
import qualified Data.Text.Lazy            as TL
import qualified Database.Redis            as R
import           GHC.Generics
import           Lib                       (generateHash)
import           Network.HTTP.Types.Status (mkStatus)
import           Network.URI               (parseURI)
import           Network.URI.Encode        (decodeBSToText, encodeTextToBS)
import           Web.Scotty

saveURI :: R.Connection
        -> BC.ByteString
        -> BC.ByteString
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set shortURI uri

getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

data APIResponse = APIReponse {
 msg :: TL.Text
} deriving (Show, Generic)

respond :: TL.Text -> ActionM ()
respond res = json $ APIReponse res

addStatus :: Int -> BC.ByteString -> ActionM ()
addStatus num str = status $ mkStatus num str

data APIRequest = APIRequest {
      url :: TL.Text
    } deriving (Show, Generic)

instance A.FromJSON APIRequest where
  parseJSON = A.withObject "APIRequest" $
    \v -> APIRequest <$> (A..:) v "url"

instance A.ToJSON APIResponse


app :: R.Connection -> ScottyM ()
app rConn = do
  post "/api" $ do
    req <- jsonData

    let uri = url req
        parsedUri = parseURI (TL.unpack uri)

    case parsedUri of
      Just _  -> do
        hash <- liftIO generateHash

        let hash' = BC.pack hash
            uriEndoced = encodeTextToBS $ TL.toStrict uri
            saveURI' = saveURI rConn hash' uriEndoced

        _ <- liftIO saveURI'
        respond $ "link: " <> TL.pack hash

      Nothing -> do
        respond $ uri <> " wasn't a url, did you forget http://?"
        addStatus 400 "Incorrect format"


  get "/api/:short" $ do
    short <- param "short"
    uri <- liftIO $ getURI rConn short

    case uri of
      Left reply -> do
          addStatus 500 "Unknown error"
          respond $ TL.pack $ show reply

      Right mbBS -> case mbBS of
        Nothing -> do
          addStatus 404 "Not found"
          respond "URI not found"

        Just bs -> respond tbs
          where tbs = TL.fromStrict $ decodeBSToText bs

  get "/" $ do
    addStatus 404 "Not found"
    respond "Incorrect route"

main :: IO ()
main = R.connect R.defaultConnectInfo >>=
    \rConn -> scotty 3000 $ app rConn
