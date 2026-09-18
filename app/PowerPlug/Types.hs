{-# LANGUAGE OverloadedStrings #-}

module PowerPlug.Types
  ( PowerPlug (..)
  , PowerStatus (..)
  ) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Maybe (catMaybes)

import Types

data PowerPlug = PowerPlug
  { setPower :: Bool -> IO (Either Error ())
  , readStatus :: IO (Either Error PowerStatus)
  }

data PowerStatus = PowerStatus
  { powerOn :: Bool
  , powerWatts :: Maybe Double
  , powerVolt :: Maybe Double
  , powerAmpere :: Maybe Double
  }

instance ToJSON PowerStatus where
  toJSON status = object $ catMaybes
    [ Just $ "on" .= powerOn status
    , ("watts" .=) <$> powerWatts status
    , ("volt" .=) <$> powerVolt status
    , ("ampere" .=) <$> powerAmpere status
    ]
