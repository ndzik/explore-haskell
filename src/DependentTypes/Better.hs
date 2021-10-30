{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-} -- For kind variable use in `OnCallBack` signature.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-} -- For `forall` use in `register`.
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-} -- For `TypeError` use in `CallBack`.
{-# LANGUAGE StandaloneKindSignatures #-}
module DependentTypes.Better where

import Data.Kind
import GHC.TypeLits
import Control.Monad

data Event = SConfig | SProof | SExit deriving Show

-- Exhaustive checks for type families TBD:
-- https://gitlab.haskell.org/ghc/ghc/-/issues/10116
type CallBack :: Event -> Type
type family CallBack ev where
  CallBack 'SConfig = (String -> IO ())
  CallBack 'SProof = (String -> IO Int)
  CallBack a = TypeError ('Text "Unimplemented type family clause for event: " ':$$: 'ShowType a )

type OnCallBack :: ev -> Type
data OnCallBack cb where
  OnConfig :: OnCallBack (CallBack 'SConfig)
  OnProof :: OnCallBack (CallBack 'SProof)
  OnExit :: OnCallBack (CallBack 'SExit)

register :: forall cb. OnCallBack cb -> cb -> IO ()
register OnConfig cb = cb "420"
register OnProof cb = void $ cb "69"
register OnExit _ = undefined
