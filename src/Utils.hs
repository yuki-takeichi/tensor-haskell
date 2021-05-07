{-# LANGUAGE TypeApplications, ScopedTypeVariables, KindSignatures, DataKinds, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{- HLINT ignore "Redundant lambda" -}
module Utils where

import Operators
import Data.HList.HList

newtype OneTuple a = OneTuple a deriving (Eq, Show)

instance Character a => Character (OneTuple a) where
  charset = map OneTuple (charset @a)

instance HTuple '[a] (OneTuple a) where
  hToTuple (x `HCons` HNil) = OneTuple x
  hFromTuple (OneTuple x) = x `HCons` HNil

tupleToHListTensor :: (HTuple (v1 :: [*]) t1, HTuple (v2 :: [*]) t2) => Tensor t1 t2 -> Tensor (HList v1) (HList v2)
tupleToHListTensor f = \t1 t2 -> f (hToTuple t1) (hToTuple t2)

hListToTupleTensor :: (HTuple (v1 :: [*]) t1, HTuple (v2 :: [*]) t2) => Tensor (HList v1) (HList v2) -> Tensor t1 t2
hListToTupleTensor f = \v1 v2 -> f (hFromTuple v1) (hFromTuple v2)

tupleToHListTensor' :: HTuple (v1 :: [*]) t1 => Tensor t1 t2 -> Tensor (HList v1) (HList '[t2])
tupleToHListTensor' f = \t1 (t2 `HCons` HNil) -> f (hToTuple t1) t2

toHListTensor :: Tensor a b -> Tensor (HList '[a]) (HList '[b])
toHListTensor f = \(a `HCons` HNil) (b `HCons` HNil) -> f a b

toHListDist :: Dist a -> HDist '[a]
toHListDist f = \HNil (a `HCons` HNil) -> f () a

isMarkov :: forall a b . (Character a, Character b) => Tensor a b -> Bool
isMarkov f = all (\a -> sigma @b (\b -> f a b) == 1) (charset @a)

funcToTensor :: Eq (HList bs) => (HList as -> HList bs) -> HTensor as bs
funcToTensor f = \as bs -> eq (f as) bs