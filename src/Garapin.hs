{-# LANGUAGE DataKinds #-}
module Garapin where

import Operators
import Data.HList.HList

data Pin = Head | Tail deriving (Show, Eq)
data Gara = Rd | Gr | Bl deriving (Show, Eq)

instance Character Pin where
  charset = [Head, Tail]

instance Character Gara where
  charset = [Rd, Gr, Bl]

tensor1 :: Tensor (HList '[]) (HList '[Pin])
tensor1 HNil (Head `HCons` HNil) = 0.6
tensor1 HNil (Tail `HCons` HNil) = 0.4

tensor2 :: Tensor (HList '[Pin]) (HList '[Gara])
tensor2 (Head `HCons` HNil) (Rd `HCons` HNil) = 0.1
tensor2 (Head `HCons` HNil) (Gr `HCons` HNil) = 0.3
tensor2 (Head `HCons` HNil) (Bl `HCons` HNil) = 0.6
tensor2 (Tail `HCons` HNil) (Rd `HCons` HNil) = 0.2
tensor2 (Tail `HCons` HNil) (Gr `HCons` HNil) = 0.3
tensor2 (Tail `HCons` HNil) (Bl `HCons` HNil) = 0.5