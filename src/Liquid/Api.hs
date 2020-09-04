{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Liquid.Api
    ( getProducts
    , getProductById
    -- , getPersonalInfo'
    -- , getPersonalInfoSigned'
    -- , getPersonalStatement'
    -- , getPersonalStatementSigned'
    -- , getPersonalStatementFull
    ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Monoid
import           Data.Proxy
import qualified Data.Text                  as T
import           Data.Time
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.HTTP.Client        (Manager, newManager)
import qualified Network.HTTP.Client        as C
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Conduit       (simpleHttp)
import qualified Network.HTTP.Simple        as S
import           Servant.API
import           Servant.Client

import qualified Liquid.Types               as LT
import           Liquid.Utils

--------------------------------------------------------------------------------

endpoint = "api.liquid.com"

type LiquidPublicAPI =

   -- GET /products
   -- GET /products?perpetual=1
       "products"
    :> Header "X-Quoine-API-Version" T.Text
    :> Get '[JSON] [LT.Product]

   -- GET /products/:id
  :<|> "products"
    :> Capture "id" Int
    :> Header "X-Quoine-API-Version" T.Text
    :> Get '[JSON] LT.Product

  -- GET /products/:id/price_levels
  :<|> "products"
    :> Capture "id" Int
    :> "price_levels"
    :> QueryParam "full" Bool
    :> Header "X-Quoine-API-Version" T.Text
    :> Get '[JSON] LT.OrderBook

liquidPublicAPI :: Proxy LiquidPublicAPI
liquidPublicAPI = Proxy

-- Derive call functions for the Api
getProducts    :: Maybe T.Text -> ClientM [LT.Product]
getProductById :: Int -> Maybe T.Text -> ClientM LT.Product
getOrderBook   :: Int -> Maybe Bool -> Maybe T.Text -> ClientM LT.OrderBook
(     getProducts
 :<|> getProductById
 :<|> getOrderBook ) = client liquidPublicAPI

--------------------------------------------------------------------------------
-- Authenticated API description

-- type LiquidAuthenticatedAPI =
-- TODO:

--------------------------------------------------------------------------------

-- | Get a basic list of monobank currency rates
-- |
getProducts' :: IO (Either ClientError [LT.Product])
getProducts' = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getProducts (Just "2")) env
  where
    host = (BaseUrl Https endpoint 443 "")

-- | Get particular product information
--   For some reason only odd ids are working, 1, 3, 5..
--   TODO: investigate ^
--
getProductById' :: Int                        -- ^ unique product id
                -> IO (Either ClientError LT.Product)
getProductById' id = do
  mgr <- newManager tlsManagerSettings
  let env = ClientEnv mgr host Nothing
  runClientM (getProductById id (Just "2")) env
    where
      host = (BaseUrl Https endpoint 443 "")

getOrderBook' = undefined
