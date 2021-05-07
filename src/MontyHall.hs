{-# LANGUAGE DataKinds, TypeApplications, ScopedTypeVariables, FlexibleContexts #-}
module MontyHall where

import Operators
import Data.HList
import Utils
import Data.Ratio

data Door = A | B | C deriving (Show, Eq)

instance Character Door where
  charset = [A, B, C]

newtype WinDoor = Win Door deriving (Show, Eq)

instance Character WinDoor where
  charset = map Win [A, B, C]

newtype GuestChoice = Guest Door deriving (Show, Eq)

instance Character GuestChoice where
  charset = map Guest [A, B, C]

newtype MontyChoice = Monty Door deriving (Show, Eq)

instance Character MontyChoice where
  charset = map Monty [A, B, C]

tmh1 :: Tensor (WinDoor, GuestChoice) MontyChoice
tmh1 (Win wd, Guest g) (Monty m) | wd == g && m == wd = 0
                                 | wd == g && m /= wd = 1/2
                                 | wd /= g && m == wd = 0
                                 | wd /= g && m == g = 0
                                 | wd /= g && otherwise = 1

mh1 = tupleToHListTensor' tmh1

uniform :: forall cs. Character (HList cs) => HDist cs
uniform HNil _ = 1 % fromIntegral (length (charset @(HList cs)))

mh2 = conversion (uniform @'[WinDoor, GuestChoice]) mh1

choose :: Door -> HDist '[Door]
choose d HNil (x `HCons` HNil) | x == d = 1
                               | otherwise = 0
                            
mh3 = choose B --> funcToTensor (hMap Monty) --> mh2

mh4 = conditionalize @HOne $ mh3 --> swap @HOne

mh5 = choose A --> funcToTensor (hMap Guest) --> mh4