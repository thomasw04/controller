{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module PowerPlug
  ( PowerPlug (..)
  , PowerStatus (..)
  , Type (..)
  , driver
  , codec
  , Site (..)
  , HasPowerPlugs (..)
  , resourcesSite
  ) where

import Data.IP (IP)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import PowerPlug.Types
import Toml (TomlCodec)
import Yesod.Core

import Types
import qualified Network.HTTP.Types.Status as Status
import qualified PowerPlug.ShellyPlugGen3
import qualified Toml

data Type = ShellyPlugGen3

instance ToJSON Type where
    toJSON = String . typeName

typeName :: Type -> Text
typeName ShellyPlugGen3 = "shelly-plug-gen3"

data Site = Site

class Yesod master => HasPowerPlugs master where
    resolvePowerPlug :: Text -> HandlerFor master PowerPlug
$(do
    let routes = [parseRoutes|
/#Text/power PowerR POST
/#Text/status StatusR GET
|]
    routeTypes <- mkYesodSubData "Site" routes
    dispatch <- [d|
        instance HasPowerPlugs master => YesodSubDispatch Site master where
            yesodSubDispatch = $(mkYesodSubDispatch routes)
        |]
    pure $ routeTypes ++ dispatch)

postPowerR :: HasPowerPlugs master => Text -> SubHandlerFor Site master TypedContent
postPowerR name = do
    plug <- liftHandler $ resolvePowerPlug name
    on <- requireCheckJsonBody
    sendRequest $ setPower plug on
    sendResponseStatus Status.status204 ("" :: Text)

getStatusR :: HasPowerPlugs master => Text -> SubHandlerFor Site master Value
getStatusR name = do
    plug <- liftHandler $ resolvePowerPlug name
    toJSON <$> sendRequest (readStatus plug)

sendRequest :: IO (Either Error a) -> SubHandlerFor Site master a
sendRequest action = do
    result <- liftIO action
    case result of
        Left Timeout ->
            sendResponseStatus Status.status504 $ object ["error" .= String "Plug request timed out"]
        Left (Failure message) ->
            sendResponseStatus Status.status502 $ object ["error" .= message]
        Right value -> pure value

driver :: Type -> Manager -> IP -> PowerPlug
driver ShellyPlugGen3 = PowerPlug.ShellyPlugGen3.powerPlug

codec :: TomlCodec Type
codec = Toml.textBy typeName parse "type"
    where
        parse "shelly-plug-gen3" = Right ShellyPlugGen3
        parse _ = Left "Unsupported power-plug type"
