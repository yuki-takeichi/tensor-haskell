{-# LANGUAGE DataKinds, TypeApplications #-}
module Monad where

import Operators
import Utils

-- 7.3.4.2 Probability distributions (Category Theory for the Sciences, Spivak)

data S = A | B | C | D deriving Show

instance Character S where
  charset = [A, B, C, D]

tM :: Tensor S S
tM A A = 0.5
tM A B = 0.5
tM A C = 0
tM A D = 0
tM B A = 0
tM B B = 1
tM B C = 0
tM B D = 0
tM C A = 0.7
tM C B = 0
tM C C = 0.3
tM C D = 0
tM D A = 0.4
tM D B = 0.3
tM D C = 0
tM D D = 0.3

tInit :: Tensor () S
tInit () A = 0.25
tInit () B = 0.25
tInit () C = 0.25
tInit () D = 0.25

f t = compose t (toHListTensor tM)

m = toHListTensor tM

rep :: (Num n, Eq n) => (a -> a) -> n -> a -> a
rep _ 0 a = a
rep f n a = rep f (n - 1) (f a)

-- TODO impl instance Monad