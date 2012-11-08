{-# LANGUAGE NoImplicitPrelude #-}
module ITMOPredule.Algebra where

-- Реализовать для всего,
-- что только можно из
import ITMOPrelude.Primitive
-- всевозможные инстансы для классов ниже 

-- Если не страшно, то реализуйте их и для
import ITMOPrelude.List
import ITMOPrelude.Tree

-- Классы
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a

class Monoid a => Group a where
    ginv :: a -> a

newtype NatSum = NatSum { getNatSum :: Nat }
newtype NatProd = NatProd { getNatProd :: Nat }

newtype IntSum = IntSum { getIntSum :: Int }
newtype IntProd = IntProd { getIntProd :: Int }

newtype RatSum = RatSum { getRatSum :: Rat }
newtype RatProd = RatProd { getRatProd :: Rat }

-- Инстансы писать сюда

instance Monoid NatSum where
mempty = NatSum natZero
mappend (NatSum a) (NatSum b) = NatSum (a +. b)

instance Monoid NatProd where
mempty = NatProd natOne
mappend (NatProd a) (NatProd b) = NatProd (a *. b)


instance Monoid IntSum where
mempty = IntSum intZero
mappend (IntSum a) (IntSum b) = IntSum (a .+. b)

instance Monoid IntProd where
mempty = IntProd intOne
mappend (IntProd a) (IntProd b) = IntProd (a .*. b)


instance Monoid RatSum where
mempty = RatSum (Rat intZero natOne)
mappend (RatSum a) (RatSum b) = RatSum (a %+ b)

instance Monoid RatProd where
mempty = RatProd (Rat intOne natOne)
mappend (RatProd a) (RatProd b) = RatProd (a %* b)


instance Group IntSum where
ginv (IntSum a) = IntSum (intNeg a)

instance Group RatSum where
ginv (RatSum a) = RatSum (ratNeg a)

instance Group RatProd where
ginv (RatProd a) = RatProduct (ratInv a)
