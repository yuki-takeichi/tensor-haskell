{-# LANGUAGE DataKinds
           , FlexibleContexts
           , ScopedTypeVariables
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
module Operators where

import Data.HList.HList
import Data.HList.HListPrelude
import Data.HList.FakePrelude

type Tensor a b = a -> b -> Rational
type HTensor as bs = Tensor (HList as) (HList bs)

type Dist a = Tensor () a
type HDist as = HTensor '[] as

eq :: Eq a => a -> a -> Rational
eq x y | x == y = 1
eq x y | otherwise = 0

sigma :: forall a . (Character a) => (a -> Rational) -> Rational
sigma f = sum $ map f (charset @a)

class Character a where
  charset :: [a]

cartesianProduct :: [a] -> [b] -> (a -> b -> c) -> [c]
cartesianProduct as bs f = as >>= (\a -> bs >>= (\b -> return $ f a b))

instance (Show a, Show b, Character a, Character b) => Show (Tensor a b) where
  show f = unlines $ cartesianProduct (charset @a) (charset @b) (\a b -> show a ++ "\t->\t" ++ show b ++ "\t->\t" ++ show (f a b))

instance (Show a, Character a) => Show (a -> Rational) where
  show f = unlines $ map (\a -> show a ++ "\t->\t" ++ show (f a)) (charset @a)

instance Character (HList '[]) where
  charset = [HNil] -- singleton

instance (Character e, Character (HList l)) => Character (HList (e ': l)) where
  charset = cartesianProduct (charset @e) (charset @(HList l)) (\e l -> e `HCons` l)

-- Partitioned asbs as bs <==> asbs == as ++ bs ∧ length as == m (in type level)
type Partitioned m asbs as bs = (
  -- constraints required by `hSplitAt`
  HSplitAt1 '[] m asbs as bs,
  HLengthEq as m,
  HAppendList1 as bs asbs,

  -- constraints required by `hAppend`
  HList asbs ~ HAppendR (HList as) (HList bs),
  HAppendList as bs
  )

tensorProduct :: (
          Partitioned m asbs as bs,
          Partitioned n csds cs ds
          ) =>
          HTensor as cs
           -> HTensor bs ds
           -> HTensor asbs csds
tensorProduct f g asbs csds = let (cs, ds) = hSplitAt Proxy csds
                                  (as, bs) = hSplitAt Proxy asbs
                              in f as cs * g bs ds

f |*| g = tensorProduct f g

i :: Eq (HList l) => HTensor l l
i = eq

cap :: forall as n asas . (
  Partitioned n asas as as,
  Eq (HList as)) => HDist asas
cap = \HNil asas -> let (as1, as2) = hSplitAt (Proxy @n) asas in eq as1 as2

cup :: forall as n asas . (
  Partitioned n asas as as,
  Eq (HList as)) => HTensor asas '[]
cup = \asas HNil -> let (as1, as2) = hSplitAt (Proxy @n) asas in eq as1 as2

f --> g = compose f g

compose :: forall as bs cs. Character (HList bs) =>
  HTensor as bs -> HTensor bs cs -> HTensor as cs
compose f g = \as cs -> sigma @(HList bs) (\bs -> f as bs * g bs cs)

instance (Character a, Character b) => Eq (Tensor a b) where
  f == g = all (\a -> all (\b -> f a b == g a b) (charset @b)) (charset @a)

-- TODO prove presevation of Markov property
jointify :: Tensor () a -> Tensor a b -> Tensor ((),()) (a, b)
jointify f g = \((), ()) (a, b) -> f () a * g a b

jointifyH :: Partitioned m asbs as bs => HDist as -> HTensor as bs -> HDist asbs
jointifyH f g = \HNil asbs -> let (as, bs) = hSplitAt Proxy asbs in f HNil as * g as bs

normalize :: forall a b. Character b => Tensor a b -> Tensor a b
normalize f = \a b -> let sum = sigma @b (\b -> f a b) in f a b

normalizeH :: forall as bs. Character (HList bs) => HTensor as bs -> HTensor as bs
normalizeH f = \as bs -> let sum = sigma @(HList bs) (\bs -> f as bs) in if sum == 0 then 0 else f as bs / sum

-- TODO prove presevation of Markov property (excersize 4 (A18))
-- explicit type applications are requires: conditionalizeH @HOne p
conditionalize :: forall m as bs asbs . (
  Character (HList bs), Partitioned m asbs as bs)
  => HDist asbs -> HTensor as bs
conditionalize f = normalizeH $ \as bs -> f HNil (hAppend as bs)

-- TODO prove presevation of Markov property (excersize 8 (A18))
marginalize1 :: forall n asbs as bs asn . (
  Partitioned n asbs as bs,
  Partitioned n asn as '[],
  as ~ asn,
  Eq (HList asn),
  Character (HList asbs)) => HDist asbs -> HDist as
marginalize1 f = f --> (i @as |*| bang @bs)

marginalize2 :: forall n asbs as bs asn . (
  Partitioned n asbs as bs,
  Eq (HList bs),
  Character (HList asbs)) => HDist asbs -> HDist bs
marginalize2 f = f --> (bang @as |*| i @bs)

bang :: Tensor (HList as) (HList '[])
bang = \as HNil -> 1

swap :: forall n asbs as bs m bsas . (
  Partitioned n asbs as bs,
  Partitioned m bsas bs as,
  Eq (HList as),
  Eq (HList bs)
  ) => HTensor asbs bsas
swap = \asbs bsas -> let (as1, bs1) = hSplitAt (Proxy @n) asbs
                         (bs2, as2) = hSplitAt (Proxy @m) bsas
                     in eq as1 as2 * eq bs1 bs2

type HOne = HSucc HZero

-- p --> swap @HOne

conversion :: forall n m bsas asbs as bs . (
  Character (HList as),
  Character (HList asbs),
  Eq (HList as),
  Eq (HList bs),
  Partitioned n bsas bs as,
  Partitioned m asbs as bs
  ) => HDist as -> HTensor as bs -> HTensor bs as
conversion p f = conditionalize @n $ (jointifyH p f --> swap @m) -- exercise 10 & 11 (A18)

hadamardProduct :: HDist as -> HDist as -> HDist as
hadamardProduct f g = \HNil as -> f HNil as * g HNil as

delta :: forall as n asas . (
  Partitioned n asas as as,
  Eq (HList as)) => HTensor as asas
delta = \as asas -> let (as1, as2) = hSplitAt (Proxy @n) asas in eq as as1 * eq as1 as2
-- delta @'[Crow]


-- TODO refine precedences
infixl 6 -->
infixl 5 |*|

type family HAdd a b where
  HAdd HZero x = x
  HAdd (HSucc x) y = HSucc (HAdd x y)

ulbend :: forall es is os n esis esos esesis eses m. (
  Partitioned n esis es is,     -- esis == es ++ is
  Partitioned n esesis es esis, -- esesis == es ++ esis
  Partitioned m esesis eses is, -- esesis == eses + is
  m ~ HAdd n n, -- typechecker unused?
  Partitioned n esos es os,     -- esos = es ++ os
  Partitioned n eses es es,     -- eses = es ++ es
  Character (HList es),
  Character (HList esesis),
  Eq (HList es),
  Character (HList is),
  Eq (HList is)
  ) => HTensor esis os -> HTensor is esos
ulbend f = compose @is @esesis @esos
           (cap @es |*| i @is)
           (i @es |*| f)

llbend :: forall n is esos esis os esesos es eses . (
  Partitioned n esos es os,
  Partitioned n esis es is,
  Partitioned n esesos es esos,
  Partitioned (HAdd n n) esesos eses os,
  Partitioned n eses es es,     -- eses = es ++ es
  Character (HList esesos),
  Eq (HList os),
  Eq (HList es)
  ) => HTensor is esos -> HTensor esis os
llbend f = compose @esis @esesos @os
           (i @es |*| f)
           (cup @es |*| i @os)

-- llbend @HOne tCrowIsBlack

urbend :: forall n m e is os ise ose isee isn. (
  Partitioned n ise is '[e],           -- ise == is ++ [e]
  Partitioned n isee is '[e, e],       -- isee == is ++ [e, e]
  Partitioned (HSucc n) isee ise '[e], -- isee == ise ++ [e]
  Partitioned m ose os '[e],           -- ose == os ++ [e]

  Partitioned n isn is '[], -- isn == is ++ []
  isn ~ is,                 -- isn == is

  Character (HList isee),
  Character e,
  Eq e,
  Character (HList is),
  Eq (HList is)
  ) => HTensor ise os -> HTensor is ose
urbend f = compose @is @isee @ose 
                   (i @is |*| cap @'[e] @(HSucc HZero) @'[e, e])
                   (f |*| (i @'[e]))

-- urbend tCrowIsBlack -- OK!

lrbend :: forall n is ises es oses os iseses isn eses m p q. (
  Partitioned n ises is es,
  Partitioned n iseses is eses,
  Partitioned m iseses ises es,
  Partitioned p eses es es,
  m ~ HAdd n p,
  Partitioned q oses os es,
  Partitioned n isn is '[], -- isn == is ++ []
  isn ~ is,                 -- isn == is
  Character (HList is),
  Character (HList iseses),
  Eq (HList es),
  Eq (HList is)
  ) => HTensor ises os -> HTensor is oses
lrbend f = compose @is @iseses @oses
                   (i @is |*| cap @es)
                   (f |*| i @es)

-- lrbend @HOne @'[Crow] tCrowIsBlack -- OK!
-- XXX (lrbend' @HOne @'[Sex] p) causes typecheker infinte loop
--   Reduction stack overflow; size = 201
--   When simplifying the following type: 'HSucc n0 ~ 'HSucc n1
