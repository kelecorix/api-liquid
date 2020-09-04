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
import           Data.Maybe
import qualified Data.Text     as T
import           Data.Time
import           GHC.Generics

import           Liquid.Utils

--------------------------------------------------------------------------------

data Product =
  Product
   { id :: Int
   } deriving (Eq, Show, Generic, FromJSON)


data OrderBook =
  OrderBook
    { obTimestamp       :: T.Text                         -- timestamp
    , obBuyPriceLevels  :: [[T.Text]]                     -- buy_price_levels
    , obSellPriceLevels :: [[T.Text]]                     -- sell_price_levels
    } deriving (Eq, Show, Generic, FromJSON)              --   Each price level follows: [price, amount]
