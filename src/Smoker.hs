module Smoker where

import Operators
import Data.HList.HList

data Pressure = H | M | L deriving Show
data Smoker = Sp | Sn deriving Show
data ASick = Ap | An deriving Show
data BSick = Bp | Bn deriving Show

instance Character Pressure where
  charset = [H, M, L]

instance Character Smoker where
  charset = [Sp, Sn]

instance Character ASick where
  charset = [Ap, An]

instance Character BSick where
  charset = [Bp, Bn]

tensor3 :: Tensor (Pressure, Smoker) (ASick, BSick)
tensor3 (H, Sp) (Ap, Bp) = 0.3
tensor3 (H, Sp) (Ap, Bn) = 0.1
tensor3 (H, Sp) (An, Bp) = 0.1
tensor3 (H, Sp) (An, Bn) = 0.5
--
tensor3 (H, Sn) (Ap, Bp) = 0.1
tensor3 (H, Sn) (Ap, Bn) = 0.1
tensor3 (H, Sn) (An, Bp) = 0.1
tensor3 (H, Sn) (An, Bn) = 0.7
--
tensor3 (M, Sp) (Ap, Bp) = 0.2
tensor3 (M, Sp) (Ap, Bn) = 0.1
tensor3 (M, Sp) (An, Bp) = 0.0
tensor3 (M, Sp) (An, Bn) = 0.7
--
tensor3 (M, Sn) (Ap, Bp) = 0.1
tensor3 (M, Sn) (Ap, Bn) = 0.1
tensor3 (M, Sn) (An, Bp) = 0.1
tensor3 (M, Sn) (An, Bn) = 0.7
--
tensor3 (L, Sp) (Ap, Bp) = 0.0
tensor3 (L, Sp) (Ap, Bn) = 0.1
tensor3 (L, Sp) (An, Bp) = 0.0
tensor3 (L, Sp) (An, Bn) = 0.9
--
tensor3 (L, Sn) (Ap, Bp) = 0.0
tensor3 (L, Sn) (Ap, Bn) = 0.0
tensor3 (L, Sn) (An, Bp) = 0.0
tensor3 (L, Sn) (An, Bn) = 1.0

tensor4 :: Tensor () Pressure
tensor4 () H = 0.1
tensor4 () M = 0.7
tensor4 () L = 0.2

tensor5 :: Tensor () Smoker
tensor5 () Sp = 0.178
tensor5 () Sn = 1 - 0.178