{-# LANGUAGE OverloadedStrings #-}

module Config
  ( Config (..)
  , Device (..)
  , Kind (..)
  , loadConfig
  ) where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Aeson (ToJSON (toJSON), object)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.IP (IP)
import Data.Map.Strict (Map)
import Data.Text (Text)
import Toml (TomlCodec, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Toml

import qualified PowerPlug

data Kind
    = PowerPlug PowerPlug.Type

data Device = Device
    { deviceName :: Text
    , deviceKind :: Kind
    , deviceIp :: IP
    }

instance ToJSON Kind where
    toJSON kind =
        let (name, typ) = case kind of
                PowerPlug t -> ("power-plug", toJSON t)
        in object
            [ "kind" Aeson..= (name :: Text)
            , "type" Aeson..= typ
            ]

instance ToJSON Device where
    toJSON device = object
        [ "name" Aeson..= deviceName device
        , "kind" Aeson..= deviceKind device
        , "ip" Aeson..= show (deviceIp device)
        ]

data Config = Config
    { configToken :: Text
    , configDevices :: Map Text Device
    }

data ConfigFile = ConfigFile
    { fileToken :: Text
    , fileDevices :: [Device]
    }

kindCodec :: TomlCodec Kind
kindCodec =
    Toml.dimatch extract PowerPlug $
        PowerPlug.codec
            <* (Toml.hardcoded ("power-plug" :: Text) Toml._Text "kind"
                .= const "power-plug")
    where
        extract (PowerPlug typ) = Just typ

deviceCodec :: TomlCodec Device
deviceCodec = Device
    <$> Toml.textBy id validateName "name" .= deviceName
    <*> kindCodec .= deviceKind
    <*> Toml.read "ip" .= deviceIp

configCodec :: TomlCodec ConfigFile
configCodec = ConfigFile
    <$> Toml.textBy id validateToken "token" .= fileToken
    <*> Toml.list deviceCodec "devices" .= fileDevices

validateToken :: Text -> Either Text Text
validateToken token
  | not (Text.null token) && Text.all tokenCharacter token = Right token
  | otherwise = Left "token must be nonempty and contain only letters, digits or -._~+/="
  where
    tokenCharacter c = alphaNumeric c || c `elem` ("-._~+/=" :: String)

validateName :: Text -> Either Text Text
validateName name = case Text.uncons name of
  Just (first, rest)
    | alphaNumeric first && Text.all nameCharacter rest -> Right name
  _ -> Left "Device names must start with a letter or digit and contain only letters, digits, - or _"
  where
    nameCharacter c = alphaNumeric c || c == '-' || c == '_'

alphaNumeric :: Char -> Bool
alphaNumeric c = isAsciiLower c || isAsciiUpper c || isDigit c

validateConfig :: ConfigFile -> Either Text Config
validateConfig decoded = do
  let devices = fileDevices decoded
      byName = Map.fromList [(deviceName device, device) | device <- devices]
  unless (Map.size byName == length devices) $ Left "Device names must be unique"
  pure $ Config (fileToken decoded) byName

loadConfig :: FilePath -> IO (Either Text Config)
loadConfig file = do
  contents <- try (BS.readFile file) :: IO (Either IOException BS.ByteString)
  pure $ case contents of
    Left err -> Left $ "Cannot read configuration: " <> Text.pack (show err)
    Right bytes -> do
      text <- either (const $ Left "Configuration must be UTF-8") Right
        (Text.decodeUtf8' bytes)
      -- Decoder errors can contain source values, including the token.
      decoded <- either (const $ Left "Invalid TOML configuration: check syntax, fields, token, device names, kinds, types and IP addresses") Right
        (Toml.decodeExact configCodec text)
      validateConfig decoded
