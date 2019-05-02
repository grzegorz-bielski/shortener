{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( generateHash,
    getURI,
    saveURI,
    respond,
    addStatus,
    url
    ) where

import           Control.Monad             (replicateM)
import qualified Data.Aeson                as A
import qualified Data.ByteString.Char8     as BC
import qualified Data.Text.Lazy            as TL
import qualified Database.Redis            as R
import           GHC.Generics
import           Network.HTTP.Types.Status (mkStatus)
import           Network.URI.Encode        (encodeTextToBS)
import qualified System.Random             as SR
import           Web.Scotty                (ActionM, json, status)

data APIResponse = APIReponse {
        msg :: TL.Text
    } deriving (Show, Generic)

data APIRequest = APIRequest {
        url :: TL.Text
    } deriving (Show, Generic)

instance A.FromJSON APIRequest where
    parseJSON = A.withObject "APIRequest" $
        \v -> APIRequest <$> (A..:) v "url"

instance A.ToJSON APIResponse

generateHash :: IO String
generateHash = hash
    where hash = replicateM num $ randomElement alphaNum
          num = 7

          alphaNum = ['A'..'Z'] <> ['0'..'9']

          randomElement xs =
            let getElement randomDigit = xs !! randomDigit
            in
            getElement <$> SR.randomRIO (0, length xs - 1)

saveURI :: R.Connection
        -> TL.Text
        -> String
        -> IO (Either R.Reply R.Status)
saveURI rConn uri hash = storeURI
    where hash' = BC.pack hash
          uriEndoced = encodeTextToBS $ TL.toStrict uri
          storeURI = R.runRedis rConn $ R.set hash' uriEndoced


getURI  :: R.Connection
        -> BC.ByteString
        -> IO (Either R.Reply (Maybe BC.ByteString))
getURI conn shortURI = R.runRedis conn $ R.get shortURI

respond :: TL.Text -> ActionM ()
respond res = json $ APIReponse res

addStatus :: Int -> BC.ByteString -> ActionM ()
addStatus num str = status $ mkStatus num str
