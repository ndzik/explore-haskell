module Main where

import           Data.Proxy
import           DependentTypes.Simple

-- functions :: Terms -> Terms
-- type classes :: Types -> Terms
-- type families :: Types -> Types
-- GADTs :: Terms -> Types

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  on OnConfig Proxy $ \c -> print c
  on OnProof Proxy $ \p -> print p
