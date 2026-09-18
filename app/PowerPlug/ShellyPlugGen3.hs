{-# LANGUAGE OverloadedStrings #-}

module PowerPlug.ShellyPlugGen3 (powerPlug) where

import Control.Exception (try)
import Control.Monad (unless)
import Data.Aeson (Value, eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseEither)
import Data.IP (IP (..))
import Data.Text (Text)
import Network.HTTP.Client
  ( HttpException (..)
  , HttpExceptionContent (..)
  , Manager
  , Request (..)
  , RequestBody (RequestBodyLBS)
  , httpLbs
  , parseRequest
  , responseBody
  , responseStatus
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusCode)
import PowerPlug.Types
import System.Timeout (timeout)
import Types (Error (..))

powerPlug :: Manager -> IP -> PowerPlug
powerPlug manager ip = PowerPlug
  { setPower = \on -> rpc manager ip "Switch.Set"
      (object ["id" .= (0 :: Int), "on" .= on])
      (withObject "Switch.Set" $ \result -> do
        _ <- result .: "was_on" :: Parser Bool
        pure ())
  , readStatus = rpc manager ip "Switch.GetStatus"
      (object ["id" .= (0 :: Int)])
      (withObject "Switch.GetStatus" $ \result -> PowerStatus
        <$> result .: "output"
        <*> result .:? "apower"
        <*> result .:? "voltage"
        <*> result .:? "current")
  }

rpc :: Manager -> IP -> Text -> Value -> (Value -> Parser a) -> IO (Either Error a)
rpc manager ip rpcMethod params parseResult = do
  -- Bound the entire request, including connection setup and reading its body.
  outcome <- timeout timeoutMicros $ try $ do
    base <- parseRequest $ "http://" <> hostAddress <> "/rpc"
    httpLbs base
      { method = "POST"
      , requestHeaders = [("Content-Type", "application/json")]
      , requestBody = RequestBodyLBS $ encode $ object
          [ "id" .= (1 :: Int)
          , "src" .= ("controller" :: Text)
          , "method" .= rpcMethod
          , "params" .= params
          ]
      , responseTimeout = responseTimeoutMicro timeoutMicros
      , redirectCount = 0
      , checkResponse = \_ _ -> pure ()
      } manager
  pure $ case outcome of
    Nothing -> Left Timeout
    Just (Left err) -> Left $ httpFailure err
    Just (Right response)
      | statusCode (responseStatus response) == 401 ->
          Left $ Failure "Shelly device authentication is enabled; this adapter expects a device without a password"
      | statusCode (responseStatus response) /= 200 ->
          Left $ Failure "Shelly returned an unsuccessful HTTP response"
      | otherwise -> do
          value <- either (const $ Left $ Failure "Shelly returned invalid JSON") Right
            (eitherDecode $ responseBody response)
          either (const $ Left $ Failure "Shelly returned an RPC error or an invalid result") Right
            (parseEither parseEnvelope value)
  where
    timeoutMicros = 5000000
    hostAddress = case ip of
      IPv4 address -> show address
      IPv6 address -> "[" <> show address <> "]"
    parseEnvelope = withObject "Shelly RPC response" $ \response -> do
      responseId <- response .: "id"
      unless (responseId == (1 :: Int)) $ fail "Unexpected response id"
      rpcError <- response .:? "error" :: Parser (Maybe Value)
      case rpcError of
        Just _ -> fail "Shelly RPC error"
        Nothing -> response .: "result" >>= parseResult

httpFailure :: HttpException -> Error
httpFailure (HttpExceptionRequest _ ConnectionTimeout) = Timeout
httpFailure (HttpExceptionRequest _ ResponseTimeout) = Timeout
httpFailure _ = Failure "Could not communicate with Shelly"
