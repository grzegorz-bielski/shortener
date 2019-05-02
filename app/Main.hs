{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy         as TL
import qualified Database.Redis         as R
import           Lib                    (addStatus, generateHash, getURI,
                                         respond, saveURI, url)
import           Network.URI            (parseURI)
import           Network.URI.Encode     (decodeBSToText)
import           Web.Scotty

getShort :: R.Connection -> ActionM ()
getShort rConn = do
  req <- jsonData

  let uri = url req
      parsedUri = parseURI $ TL.unpack uri

      saveToDB = do
        hash <- liftIO $ generateHash
        _ <- liftIO $ saveURI rConn uri hash
        respond $ TL.pack hash

      incorrectURI =  do
        respond $ uri <> " wasn't a url, did you forget the http://?"
        addStatus 400 "Incorrect format"

  case parsedUri of
    Just _  -> saveToDB
    Nothing -> incorrectURI

saveShort :: R.Connection -> ActionM ()
saveShort rConn = do
  short <- param "short"
  uri <- liftIO $ getURI rConn short

  let dbError reply = do
        addStatus 500 "Unknown error"
        respond $ TL.pack $ show reply

      dbNotFound = do
        addStatus 404 "Not found"
        respond "URI not found"

      dbFound bs = respond tbs
        where tbs = TL.fromStrict $ decodeBSToText bs

  case uri of
    Left reply -> dbError reply

    Right maybeBS -> case maybeBS of
      Nothing -> dbNotFound
      Just bs -> dbFound bs

notFoundRoute :: ActionM ()
notFoundRoute = addStatus 404 "Not found" >> respond "Incorrect route"

app :: R.Connection -> ScottyM ()
app rConn = do
  post "/api" $ getShort rConn
  get "/api/:short" $ saveShort rConn
  get "/" notFoundRoute

main :: IO ()
main = R.connect R.defaultConnectInfo >>=
    \rConn -> scotty 3000 $ app rConn
