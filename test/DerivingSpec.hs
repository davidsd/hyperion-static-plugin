{-# OPTIONS_GHC -fplugin=Hyperion.Static.Plugin #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Hyperion.Static

data Foo = MkFoo Int
  deriving (Eq, Generic, Show, Binary, Static Binary, Static Eq, Static Show)

fooDict :: Closure (Dict (Binary Foo))
fooDict = closureDict

fooShowDict :: Closure (Dict (Show Foo))
fooShowDict = closureDict

fooEqDict :: Closure (Dict (Eq Foo))
fooEqDict = closureDict

newtype Box a = Box a
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary, Static Binary, Static Eq, Static Show)

boxDict :: Closure (Dict (Binary (Box Foo)))
boxDict = closureDict

boxShowDict :: Closure (Dict (Show (Box Foo)))
boxShowDict = closureDict

boxEqDict :: Closure (Dict (Eq (Box Foo)))
boxEqDict = closureDict

newtype ShorthandBox a = ShorthandBox a
  deriving stock Generic
  deriving anyclass (Binary, Static Binary)

shorthandBoxDict :: Closure (Dict (Binary (ShorthandBox Foo)))
shorthandBoxDict = closureDict

data Standalone = Standalone Int
  deriving stock Generic
  deriving anyclass Binary

deriving instance Static (Binary Standalone)

standaloneDict :: Closure (Dict (Binary Standalone))
standaloneDict = closureDict

data StandaloneShow = StandaloneShow Int
  deriving stock Show

deriving instance Static (Show StandaloneShow)

standaloneShowDict :: Closure (Dict (Show StandaloneShow))
standaloneShowDict = closureDict

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

data Triple a b c = Triple a b c
  deriving stock (Eq, Generic, Show)
  deriving anyclass (Binary, Static Binary, Static Eq, Static Show)

tripleBinaryDict :: Closure (Dict (Binary (Triple Int Bool ())))
tripleBinaryDict = closureDict

tripleShowDict :: Closure (Dict (Show (Triple Int Bool ())))
tripleShowDict = closureDict

tripleEqDict :: Closure (Dict (Eq (Triple Int Bool ())))
tripleEqDict = closureDict

data NoStrategy = NoStrategy Int
  deriving (Eq, Generic, Binary, Static Eq, Static Binary)

noStrategyEqDict :: Closure (Dict (Eq NoStrategy))
noStrategyEqDict = closureDict

noStrategyBinaryDict :: Closure (Dict (Binary NoStrategy))
noStrategyBinaryDict = closureDict

main :: IO ()
main = pure ()
