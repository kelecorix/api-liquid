{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}


module Liquid.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types     (emptyObject)
import           Data.ByteString.Lazy (ByteString)
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.Time
import           Data.Vector          (Vector)
import           GHC.Generics

import           Liquid.Utils

--------------------------------------------------------------------------------

data Product = Product
    { prId                  :: Text
    , prType                :: Text
    , prCode                :: Text
    , prName                :: Text
    , prMarketAsk           :: Text
    , prMarketBid           :: Text
    , prIndicator           :: Int
    , prCurrency            :: Text
    , prCurrencyPairCode    :: Text
    , prSymbol              :: Text
    , prFiatMinimumWithdraw :: Maybe Text
    , prPusherChannel       :: Text
    , prTakerFee            :: Text
    , prMakerFee            :: Text
    , prLowMarketBid        :: Text
    , prHighMarketAsk       :: Text
    , prVolume24h           :: Text
    , prLastPrice24h        :: Text
    , prLastTradedPrice     :: Text
    , prLastTradedQuantity  :: Text
    , prQuotedCurrency      :: Text
    , prBaseCurrency        :: Text
    , prExchangeRate        :: Int
    , prTimestamp           :: Text
    } deriving (Show)

decodeProduct :: ByteString -> Maybe Product
decodeProduct = decode

instance ToJSON Product where
    toJSON (Product prId prType prCode prName prMarketAsk prMarketBid prIndicator prCurrency prCurrencyPairCode prSymbol prFiatMinimumWithdraw prPusherChannel prTakerFee prMakerFee prLowMarketBid prHighMarketAsk prVolume24h prLastPrice24h prLastTradedPrice prLastTradedQuantity prQuotedCurrency prBaseCurrency prExchangeRate prTimestamp) =
        object
        [ "id" .= prId
        , "product_type" .= prType
        , "code" .= prCode
        , "name" .= prName
        , "market_ask" .= prMarketAsk
        , "market_bid" .= prMarketBid
        , "indicator" .= prIndicator
        , "currency" .= prCurrency
        , "currency_pair_code" .= prCurrencyPairCode
        , "symbol" .= prSymbol
        , "fiat_minimum_withdraw" .= prFiatMinimumWithdraw
        , "pusher_channel" .= prPusherChannel
        , "taker_fee" .= prTakerFee
        , "maker_fee" .= prMakerFee
        , "low_market_bid" .= prLowMarketBid
        , "high_market_ask" .= prHighMarketAsk
        , "volume_24h" .= prVolume24h
        , "last_price_24h" .= prLastPrice24h
        , "last_traded_price" .= prLastTradedPrice
        , "last_traded_quantity" .= prLastTradedQuantity
        , "quoted_currency" .= prQuotedCurrency
        , "base_currency" .= prBaseCurrency
        , "exchange_rate" .= prExchangeRate
        , "timestamp" .= prTimestamp
        ]

instance FromJSON Product where
    parseJSON (Object v) = Product
        <$> v .: "id"
        <*> v .: "product_type"
        <*> v .: "code"
        <*> v .: "name"
        <*> v .: "market_ask"
        <*> v .: "market_bid"
        <*> v .: "indicator"
        <*> v .: "currency"
        <*> v .: "currency_pair_code"
        <*> v .: "symbol"
        <*> v .:? "fiat_minimum_withdraw"
        <*> v .: "pusher_channel"
        <*> v .: "taker_fee"
        <*> v .: "maker_fee"
        <*> v .: "low_market_bid"
        <*> v .: "high_market_ask"
        <*> v .: "volume_24h"
        <*> v .: "last_price_24h"
        <*> v .: "last_traded_price"
        <*> v .: "last_traded_quantity"
        <*> v .: "quoted_currency"
        <*> v .: "base_currency"
        <*> v .: "exchange_rate"
        <*> v .: "timestamp"


data OrderBook =
  OrderBook
    { obTimestamp       :: T.Text                         -- timestamp
    , obBuyPriceLevels  :: [[T.Text]]                     -- buy_price_levels
    , obSellPriceLevels :: [[T.Text]]                     -- sell_price_levels
    } deriving (Eq, Show, Generic, FromJSON)              --   Each price level follows: [price, amount]
