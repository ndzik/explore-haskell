{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Test where

import Data.Dynamic
import Data.Kind
import Type.Reflection
import Data.Functor.Identity
import GHC.TypeLits

data Event = Config | Proof deriving Show

-- data CallBack e where
--   OnConfig :: CallBack (String -> IO ())
--   OnProof :: CallBack (String -> IO Int)
--
-- handlers :: CallBack cb -> cb
-- handlers OnConfig = print
-- handlers OnProof = \s -> print s >> return 420

data OneToThree a b c as where
  One :: OneToThree a b c '[a]
  Two :: OneToThree a b c [a, b]
  Three :: OneToThree a b c [a, b, c]

type family IsTypeLit a where
  IsTypeLit Nat = 'True
  IsTypeLit Symbol = 'True
  IsTypeLit _ = 'False

data T :: forall a. (IsTypeLit a ~ 'True) => a -> Type where
  MkNat :: T 42
  MkSymbol :: T "Do not panic"

-- F is a polykinded type where the first TypeLevel function goes from any kind
-- 'k' to `Type`.
type family F :: (k -> v) -> Type -> Type

-- type X = F String Int
data Callback (ev :: Event)
data YAYA (a :: Type)

type Y = F IO Int
type Z = F Callback Int

-- data Callback :: forall ev. (IsEventHandler ev ~ 'True) => ev -> Type where

-- f :: forall ev cb. (IsEventHandler ev cb ~ 'True) => ev -> cb -> ()
-- f = undefined

type family F1 :: k -> Type -- arity 1
type instance F1 = Callback

type family F0 :: forall k. k -> Type -- arity 0

type HRK (f :: forall k. k -> Type) = (f Int, f Maybe, f 'True)
type H1 = HRK F0

-- type Append :: forall a. [a] -> [a] -> [a]
-- type family Append as bs :: [a] where
type family Append as bs where
  Append '[] bs = bs
  Append (a:as) bs = a : Append as bs

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe d x where
  FromMaybe d 'Nothing = d
  FromMaybe _ ('Just x) = x

type family MaybeIf b t where
  MaybeIf 'True t = Maybe t
  MaybeIf 'False t = Identity t

data PlayerInfo b = PlayerInfo {
      name :: MaybeIf b String
    , age :: MaybeIf b Int
}

type PX = PlayerInfo 'True
type PY = PlayerInfo 'False

f :: PX -> ()
f (PlayerInfo (Just _name) (Just _age)) = undefined
f (PlayerInfo Nothing _) = undefined
f (PlayerInfo _ Nothing) = undefined

g :: PY -> IO ()
g (PlayerInfo s _) = print s

-- type family {NAME [PARAMS] = RESULT} | FUNCTIONAL DEPENDENCY.
-- Not x = r && r determines x.
-- !!ONLY guides type inference!!
type family Not x = r | r -> x where
  Not 'True = 'False
  Not 'False = 'True

ss :: forall x. (Not x ~ 'True, Typeable x) => String
ss = show (typeRep @x)

type family DeduceCallback ev where
  DeduceCallback 'Config = (String -> IO ())
  DeduceCallback 'Proof = (Int -> IO ())

-- data EventS = SConfig | SProof deriving Show
--
-- type EventF :: EventS -> Type
-- type family EventF ev = cb | cb -> ev where
--   EventF 'SConfig = (String -> IO ())
--   EventF 'SProof = (String -> IO Int)
--
-- data EMatch cb where
--   OnConfig :: EMatch (EventF 'SConfig)
--   OnProof :: EMatch (EventF 'SProof)
--
-- onn :: forall ev cb. (EventF ev ~ cb) => cb -> ()
-- onn cb = undefined
--
-- test = onn @SConfig $ \s -> print s

