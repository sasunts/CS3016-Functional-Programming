{- sasuntsv Vahe Sasunts -}
module Ex02 where

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype
data Tree k d
  = Br (Tree k d) (Tree k d) k d
  | Leaf k d
  | Nil
  deriving (Eq, Show)

type IntFun = Tree Int Int -- binary tree with integer keys and data

data Expr
  = Val Double
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Abs Expr
  | Sign Expr
   deriving (Eq, Show)



-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> Tree k d -> Tree k d
ins k d Nil = Leaf k d

ins k1 d1 (Leaf k2 d2)
  |k1 == k2 = Leaf k1 d1
  |k1 < k2  = Br (Leaf k1 d1) Nil k2 d2
  |k1 > k2  = Br Nil (Leaf k1 d1) k2 d2

ins k1 d1 (Br l r k2 d2)
  |k1 == k2 = Br l r k1 d1
  |k1 < k2  = Br (ins k1 d1 l) r k2 d2
  |k1 > k2  = Br l (ins k1 d1 r) k2 d2


-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => Tree k d -> k -> m d
lkp Nil _ = fail "Nothing"

lkp (Leaf k d) key
  |key == k  = return d
  |otherwise = fail "Wrong key"

lkp (Br l r k d) key
  |key == k = return d
  |key < k  = lkp l key
  |key > k  = lkp r key

-- Part 3 : Instance of Num for Expr

{-
  Fix the following instance for Num of Expr so all tests pass

  Note that the tests expect simplification to be done
  only when *all* Expr arguments are of the form Val v

  Hint 1 :  implementing fromInteger *first* is recommended!
  Hint 2 :  remember that Double is already an instance of Num
-}

instance Num Expr where
  e1 + e2 = case (e1,e2) of
            (Val e1, Val e2) -> Val(e1+e2)
            (_,_) -> Add e1 e2
  e1 - e2 = case (e1,e2) of
            (Val e1, Val e2) -> Val(e1-e2)
            (_,_) -> Sub e1 e2
  e1 * e2 = case (e1,e2) of
            (Val e1, Val e2) -> Val(e1*e2)
            (_,_) -> Mul e1 e2
  negate e = case (e) of
             (Val e) -> Val(-e)
             (_) -> Sub 0 e
  abs e = case (e) of
          (Val e) -> if e >= 0 then Val(e) else Val(-e)
          (_) -> Abs e
  signum e = case (e) of
              (Val e) -> if e > 0 then Val(1) else if e<0 then Val(-1) else Val(0)
              (_) -> Sign e
  fromInteger i = (Val (fromIntegral i))
