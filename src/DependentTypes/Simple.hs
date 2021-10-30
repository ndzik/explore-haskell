{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module DependentTypes.Simple where

import Data.Proxy

-- Different parameter structs which are given to eventhandlers.
data ConfigStruct = ConfigStruct { url :: String, chainId :: Int } deriving (Show, Eq)
newtype ProofStruct = ProofStruct { sig :: String } deriving (Show, Eq)

-- Associated event types for each necessary event.
data Events = TConfig | TProof deriving (Show, Eq)

-- Event is a `proof term` because the only thing it is responsible for is
-- proving to GHC that whenever `e` is of a specific `kind`, then `cb` has the
-- given function signature.
data Event e cb where
  OnConfig :: Event 'TConfig (ConfigStruct -> IO ())
  OnProof :: Event 'TProof (ProofStruct -> IO ())

-- on is dependently typed in both directions:
--  User: Inputs `on OnConfig Proxy $ cb` and knows the type is `ConfigStruct -> IO ()`
--  Dev: Pattern matches on proof term members `Event e cb` and then knows that
--  `input` is of a specific type.
--    on OnSomeEvent _ cb = cb $ input
on :: Event ev cb -> Proxy ev -> cb -> IO ()
on OnConfig _ cb = cb $ ConfigStruct "some config" 2134
on OnProof _ cb = cb $ ProofStruct "some proof"
