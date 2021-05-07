{-# LANGUAGE DataKinds, TypeApplications #-}
module SilhoutteQuiz where

import Operators
import Utils

import Data.HList

-- B19

data Height = Hi | Md | Lo deriving (Show, Eq)

instance Character Height where
  charset = [Hi, Md, Lo]

data Sex = Male | Female deriving (Show, Eq)

instance Character Sex where
  charset = [Male, Female]

data Skirt = Skp | Skn deriving (Show, Eq)

instance Character Skirt where
  charset = [Skp, Skn]

data Cane = Cap | Can deriving (Show, Eq)

instance Character Cane where
  charset = [Cap, Can]

data Elderly = Ep | En deriving (Show, Eq)

instance Character Elderly where
  charset = [Ep, En]

tH :: Tensor Sex Height
tH Male Hi = 0.2
tH Male Md = 0.6
tH Male Lo = 0.2
tH Female Hi = 0.1
tH Female Md = 0.5
tH Female Lo = 0.4

tS :: Tensor Sex Skirt
tS Male Skp = 0
tS Male Skn = 1
tS Female Skp = 0.5
tS Female Skn = 0.5

tC :: Tensor Elderly Cane
tC Ep Cap = 0.2
tC Ep Can = 0.8
tC En Cap = 0
tC En Can = 1

tP :: Tensor () (Sex, Elderly)
tP () (Male, Ep) = 0.1
tP () (Male, En) = 0.4
tP () (Female, Ep) = 0.15
tP () (Female, En) = 0.35

data Person = OldMan | Man | OldWmn | Woman deriving (Show, Eq)

instance Character Person where
  charset = [OldMan, Man, OldWmn, Woman]

tN :: Tensor (Sex, Elderly) Person
tN = \t p -> eq (n t) p
  where
    n (Male, Ep) = OldMan
    n (Male, En) = Man
    n (Female, Ep) = OldWmn
    n (Female, En) = Woman


p = tupleToHListTensor tP
h = toHListTensor tH
s = toHListTensor tS
c = toHListTensor tC
n :: Tensor (HList '[Sex, Elderly]) (HList '[Person])
n = tupleToHListTensor' tN

tSilhoutteQuiz = conversion p (((delta @'[Sex] @(HSucc HZero) @'[Sex, Sex]) --> (h |*| s)) |*| c) --> n

-- B19 exercise 12

a1 = hListToTupleTensor tSilhoutteQuiz (Hi, Skn, Can)
a2 = hListToTupleTensor tSilhoutteQuiz (Lo, Skn, Cap)
a3 = hListToTupleTensor tSilhoutteQuiz (Hi, Skn, Cap)
a4 = hListToTupleTensor tSilhoutteQuiz (Md, Skp, Cap)