{-#LANGUAGE OverloadedStrings #-}
module Main where

import Web.Scotty
import Data.TZworld.Api
import Network.HTTP.Types.Status
import qualified Data.ByteString as BS
import Control.Monad.IO.Class
import qualified Data.Text.Lazy as T

main::IO()
main = scotty 8002 $ do
  defaultHandler sendBadRequest
  get "/location" $ do 
    latp <- param "lat"::ActionM BS.ByteString
    lonp <- param "lon"::ActionM BS.ByteString
    case (BS.null latp,BS.null lonp) of
       (True,True) -> sendBadRequest "The latitude and longitude parameters are invalid"
       (True,_)    -> sendBadRequest "The latitude value is invalid"
       (_,True)    -> sendBadRequest "The longitude value is invalid"
       _           -> do
         tze <- liftIO $ handleLocation latp lonp
         case tze  of
           Left err    -> sendBadRequest $ T.pack err
           Right tzstr -> json tzstr
       
  notFound $
    sendBadRequest "The request did not include a location e.g. /location?lat=49.00&lon=-130.0"


sendBadRequest::T.Text -> ActionM ()
sendBadRequest msg = do
  status status400
  json $ Message (T.unpack msg)
  return () 
