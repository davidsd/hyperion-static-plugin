{-# OPTIONS_GHC -fplugin=Hyperion.Static.Plugin #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat, Nat)
import Hyperion.Static

-- Unambiguous datatype deriving can keep the concise, strategy-free form.
-- The plugin removes the `Static ...` markers and generates concrete
-- `Static (Binary Foo)`, `Static (Eq Foo)`, and `Static (Show Foo)` instances.
data Foo = MkFoo Int
  deriving (Eq, Generic, Show, Binary, Static Binary, Static Eq, Static Show)

fooDict :: Closure (Dict (Binary Foo))
fooDict = closureDict

fooShowDict :: Closure (Dict (Show Foo))
fooShowDict = closureDict

fooEqDict :: Closure (Dict (Eq Foo))
fooEqDict = closureDict

-- Parametric newtypes usually need explicit deriving strategies for the
-- ordinary classes. The plugin asks TH to reify GHC's inferred contexts, so
-- these `Static` instances get constraints such as `Static (Binary a)`.
newtype Box a = Box a
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary, Static Binary, Static Eq, Static Show)

boxDict :: Closure (Dict (Binary (Box Foo)))
boxDict = closureDict

boxShowDict :: Closure (Dict (Show (Box Foo)))
boxShowDict = closureDict

boxEqDict :: Closure (Dict (Eq (Box Foo)))
boxEqDict = closureDict

-- Standalone deriving is still supported for classes that are not named in a
-- datatype deriving clause.
data Standalone = Standalone Int
  deriving stock Generic
  deriving anyclass Binary

deriving instance Static (Binary Standalone)

standaloneDict :: Closure (Dict (Binary Standalone))
standaloneDict = closureDict

-- Standalone deriving is not specific to Binary.
data StandaloneShow = StandaloneShow Int
  deriving stock Show

deriving instance Static (Show StandaloneShow)

standaloneShowDict :: Closure (Dict (Show StandaloneShow))
standaloneShowDict = closureDict

-- A type with two parameters exercises inferred contexts for several ordinary
-- classes, including one derived with a stock Ord instance.
data Pair a b = Pair a b
  deriving stock (Eq, Generic, Ord, Show)
  deriving anyclass (Binary, Static Binary, Static Eq, Static Ord, Static Show)

pairBinaryDict :: Closure (Dict (Binary (Pair Foo Standalone)))
pairBinaryDict = closureDict

pairShowDict :: Closure (Dict (Show (Pair Foo StandaloneShow)))
pairShowDict = closureDict

pairEqDict :: Closure (Dict (Eq (Pair Foo Foo)))
pairEqDict = closureDict

pairOrdDict :: Closure (Dict (Ord (Pair Int Bool)))
pairOrdDict = closureDict

-- A type with three parameters checks that the generated target type is formed
-- correctly beyond the unary and binary cases.
data Triple a b c = Triple a b c
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary, Static Binary, Static Eq, Static Show)

tripleBinaryDict :: Closure (Dict (Binary (Triple Int Bool ())))
tripleBinaryDict = closureDict

tripleShowDict :: Closure (Dict (Show (Triple Int Bool ())))
tripleShowDict = closureDict

tripleEqDict :: Closure (Dict (Eq (Triple Int Bool ())))
tripleEqDict = closureDict

-- Variables explicitly kinded as Nat should get KnownNat constraints, not
-- Typeable constraints. With -Wsimplifiable-class-constraints enabled, emitting
-- Typeable p here is a warning; the test suite treats that warning as an error.
data NatBox (p :: Nat) = NatBox
  deriving stock Generic
  deriving anyclass (Binary, Static Binary)

natBoxDict :: Closure (Dict (Binary (NatBox 3)))
natBoxDict = closureDict

-- Mixed ordinary and Nat-kinded parameters should keep Static constraints for
-- ordinary payloads while using KnownNat for the Nat parameter.
data NatPair a (p :: Nat) = NatPair a
  deriving stock Generic
  deriving anyclass (Binary, Static Binary)

natPairDict :: Closure (Dict (Binary (NatPair Foo 5)))
natPairDict = closureDict

-- The standalone-deriving path has its own context-generation logic. It should
-- simplify Static (KnownNat p) to KnownNat p and avoid a redundant Typeable p.
data StandaloneNat (p :: Nat) = StandaloneNat
  deriving stock Generic
  deriving anyclass Binary

deriving instance KnownNat p => Static (Binary (StandaloneNat p))

standaloneNatDict :: Closure (Dict (Binary (StandaloneNat 7)))
standaloneNatDict = closureDict

main :: IO ()
main = pure ()
