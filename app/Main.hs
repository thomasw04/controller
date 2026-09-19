{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main, Widget, resourcesApp) where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar)
import Data.Text (Text)
import PowerPlug (HasPowerPlugs (..))
import System.Environment (lookupEnv)
import System.Exit (die)
import Yesod.Core
import qualified Config
import qualified Control.Monad as Monad
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString.Char8 as BS
import qualified Data.Char as Char
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Types.Status as Status
import qualified PowerPlug

data App = App
  { appConfig :: MVar Config.Config
  , appConfigPath :: FilePath
  , appManager :: HTTP.Manager
  }

mkYesod "App" [parseRoutes|
/health HealthR GET
/reload ReloadR POST
/list ListR GET
/power-plug PowerPlugR PowerPlug.Site getPowerPlugSite
|]

instance Yesod App where
  makeSessionBackend _ = pure Nothing

  yesodMiddleware handler = do
    addHeader "Cache-Control" "no-store"
    route <- getCurrentRoute
    case route of
      Just HealthR -> pure ()
      _ -> Monad.void authenticatedConfig
    defaultYesodMiddleware handler

  errorHandler NotFound = pure $ toTypedContent $ object ["error" .= String "Not found"]
  errorHandler (BadMethod _) = pure $ toTypedContent $ object ["error" .= String "Method not allowed"]
  errorHandler (InvalidArgs messages) = pure $ toTypedContent $ object ["error" .= Text.intercalate "; " messages]
  errorHandler err = defaultErrorHandler err

getHealthR :: Handler Value
getHealthR = pure $ object ["status" .= String "ok"]

authenticatedConfig :: Handler Config.Config
authenticatedConfig = cached $ do
  foundation <- getYesod
  configuration <- liftIO $ readMVar $ appConfig foundation
  header <- lookupHeader "Authorization"
  Monad.unless (authorized configuration header) unauthorized
  pure configuration

authorized :: Config.Config -> Maybe BS.ByteString -> Bool
authorized configuration header = case BS.words <$> header of
  Just [scheme, supplied] | BS.map Char.toLower scheme == "bearer" ->
    ByteArray.constEq supplied (Text.encodeUtf8 $ Config.configToken configuration)
  _ -> False

unauthorized :: Handler a
unauthorized = do
  addHeader "WWW-Authenticate" "Bearer"
  sendResponseStatus Status.status401 $ object ["error" .= String "Unauthorized"]

postReloadR :: Handler Value
postReloadR = do
  foundation <- getYesod
  header <- lookupHeader "Authorization"
  -- Serialize reloads, and recheck the token in case another reload rotated it.
  result <- liftIO $ modifyMVar (appConfig foundation) $ \current ->
    if not (authorized current header)
      then pure (current, Left (Status.status401, "Unauthorized" :: Text))
      else do
        loaded <- Config.loadConfig $ appConfigPath foundation
        pure $ case loaded of
          Left message -> (current, Left (Status.status400, message))
          Right replacement -> (replacement, Right ())
  case result of
    Left (status, message)
      | status == Status.status401 -> unauthorized
      | otherwise -> sendResponseStatus status $ object ["error" .= message]
    Right () -> pure $ object ["status" .= String "reloaded"]

getListR :: Handler Value
getListR = do
  configuration <- authenticatedConfig
  pure $ object ["devices" .= Config.configDevices configuration]

getPowerPlugSite :: App -> PowerPlug.Site
getPowerPlugSite _ = PowerPlug.Site

instance HasPowerPlugs App where
  resolvePowerPlug name = do
    configuration <- authenticatedConfig
    foundation <- getYesod
    case Map.lookup name (Config.configDevices configuration) of
      Nothing -> sendResponseStatus Status.status404 $ object ["error" .= String "Unknown power plug"]
      Just device -> case Config.deviceKind device of
        Config.PowerPlug typ ->
          pure $ PowerPlug.driver typ (appManager foundation) (Config.deviceIp device)

main :: IO ()
main = do
  file <- maybe "/run/secrets/config.toml" id <$> lookupEnv "CONTROLLER_CONFIG"
  loaded <- Config.loadConfig file
  configuration <- either (die . Text.unpack) pure loaded
  state <- newMVar configuration
  manager <- HTTP.newManager $ HTTP.managerSetProxy HTTP.noProxy HTTP.defaultManagerSettings
  warp 8080 $ App state file manager
