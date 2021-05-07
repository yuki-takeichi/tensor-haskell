{-# LANGUAGE DataKinds, TypeApplications #-}
module Crow where

import Operators
import Data.HList.HList
import Utils

data Crow = Cp | Cn deriving (Show, Eq)
data Black = Blp | Bln deriving (Show, Eq)

instance Character Crow where
  charset = [Cp, Cn]

instance Character Black where
  charset = [Blp, Bln]

tCrowIsBlack :: Tensor Crow Black
tCrowIsBlack Cp Blp = 1
tCrowIsBlack Cp Bln = 0
tCrowIsBlack Cn Blp = 0.4
tCrowIsBlack Cn Bln = 0.6

crowIsBlack = toHListTensor tCrowIsBlack

tCrow :: Dist Crow
tCrow () Cp = 0.4
tCrow () Cn = 0.6

crow = toHListDist tCrow

postCrow :: Tensor () Crow
postCrow () Cp = 0.4
postCrow () Cn = 0.6

blackIsCrow = conversion crow crowIsBlack

--
-- A18 exercise 13
--

data Size = Big | Small deriving (Show, Eq)

instance Character Size where
  charset = [Big, Small]

crowIsBig :: Tensor Crow Size
crowIsBig Cp Big = 0.3
crowIsBig Cp Small = 0.7
crowIsBig Cn Big = 0.1
crowIsBig Cn Small = 0.9

exercise13 = (delta @'[Crow]) --> (crowIsBlack |*| toHListTensor crowIsBig)