module A16 where

import Operators

-- exercise 1
data S1 = V deriving Show
data S2 = Vp | Vn deriving Show
data S3 = Vh | Vm | Vl deriving Show

instance Character S1 where
  charset = [V]

instance Character S2 where
  charset = [Vp, Vn]

instance Character S3 where
  charset = [Vh, Vm, Vl]

ta :: Tensor S2 S2
ta Vp Vp = 0.1
ta Vp Vn = 0.9
ta Vn Vp = 0.7
ta Vn Vn = 0.3

tb :: Tensor () (S2, S2)
tb () (Vp, Vp) = 0.1
tb () (Vp, Vn) = 0.9
tb () (Vn, Vp) = 0.7
tb () (Vn, Vn) = 0.3

tc :: Tensor () S2
tc () Vp = 0.4
tc () Vn = 0.6

td :: Tensor S2 ()
td Vp () = 0.4
td Vn () = 0.6

te :: Tensor S2 S1
te Vp V = 0.6
te Vn V = 0.4

-- partial function
tf :: Tensor S3 S2
tf Vh Vp = 0.5
tf Vm Vp = 1
tf Vl Vp = 0.9

-- partial function
tg :: Tensor (S2, S2) S2
tg (Vp, Vp) Vp = 0.5
tg (Vp, Vn) Vp = 0.1
tg (Vn, Vp) Vp = 0.9
tg (Vn, Vn) Vp = 0.5

th :: Tensor S2 (S2, S2)
th Vp (Vp, Vp) = 3
th Vp (Vp, Vn) = 2
th Vp (Vn, Vp) = 1
th Vp (Vn, Vn) = 0
th Vn (Vp, Vp) = 1
th Vn (Vp, Vn) = 1
th Vn (Vn, Vp) = 1
th Vn (Vn, Vn) = 0