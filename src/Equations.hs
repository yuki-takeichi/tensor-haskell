{-# LANGUAGE DataKinds
           , TypeApplications
           , FlexibleInstances
           , TypeOperators
           , AllowAmbiguousTypes
           , ConstraintKinds
           , TypeFamilies
#-}
{- HLINT ignore "Redundant lambda" -}
{- HLINT ignore "Avoid lambda" -}
{- HLINT ignore "Redundant bracket" -}
module Equations where

import Data.HList.HList
import Data.HList.HListPrelude
import Data.HList.FakePrelude
import Data.Proxy

import Operators
import Crow
import SilhoutteQuiz

-- 

-- A20ans related equations

e1 = swap @HOne @'[Crow, Black] --> swap @HOne @'[Black, Crow] == i @'[Crow, Black]
-- |
-- >>> e1
-- True

e2 = ((h |*| c) --> swap @HOne @'[Height, Cane]) == (swap @HOne @'[Sex, Elderly] --> (c |*| h))
-- |
-- >>> e2
-- True

e3 = (cap @'[Crow, Black]) == ((cap @'[Crow] |*| cap @'[Black]) --> ((i @'[Crow] |*| swap @HOne @'[Crow, Black]) |*| i @'[Black]))
-- |
-- >>> e3
-- True

e4 = crowIsBlack --> bang @'[Black] == bang @'[Crow]
-- |
-- >>> e4
-- True

e5 = delta @'[Crow] --> (bang @'[Crow] |*| i @'[Crow]) == i @'[Crow]
-- |
-- >>> e5
-- True

e6 = delta @'[Crow] --> (delta @'[Crow] |*| i @'[Crow]) == delta @'[Crow] --> (i @'[Crow] |*| delta @'[Crow])
-- >>> e6
-- True

e7 = cap @'[Crow] --> swap @HOne == cap @'[Crow]
-- >>> e7
-- True

-- snake equations

es1 = (cap @'[Crow] |*| i @'[Crow]) --> (i @'[Crow] |*| cup @'[Crow]) == i @'[Crow]
-- >>> es1
-- True

es2 = (i @'[Crow] |*| cap @'[Crow]) --> (cup @'[Crow] |*| i @'[Crow]) == i @'[Crow]
-- >>> es2
-- True
