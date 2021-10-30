{-# LANGUAGE GADTs #-}
module DependentTypes.Betterer where

import Control.Monad

data CB cb where
  Config :: CB (String -> IO ())
  Proof :: CB (String -> IO Int)
  Exit :: CB (String -> IO Bool)

on :: CB cb -> cb -> IO ()
on Config cb = cb "hello"
on Proof cb = void $ cb "420"
on Exit cb = void $ cb "False"

test :: IO ()
test = do
  on Config $ \s -> print s
  on Proof $ \s -> print s >> return 420
  on Exit $ \s -> print s >> return False
