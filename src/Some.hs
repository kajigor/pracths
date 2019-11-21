module Some where

import Type.Reflection ((:~~:) (HRefl), Typeable, eqTypeRep, typeOf, typeRep)


data Some' = forall a. MkSome' a

data Some c where
  MkSome :: c a => a -> Some c


showSome :: Some Show -> String
showSome (MkSome x) = show x


type ShowAndNum t = (Show t, Num t)

class ShowAndNum t => ShowAndNumClass t
instance ShowAndNum t => ShowAndNumClass t

showableList :: [Some ShowAndNumClass]
showableList = [MkSome (1 :: Int), MkSome (2 :: Integer)]



data Variant where
  MkVariant :: Typeable a => a -> Variant

showVariantType :: Variant -> String
showVariantType (MkVariant x) = show (typeOf x)

fromVariant :: forall b. Typeable b => Variant -> Maybe b
fromVariant (MkVariant x) =
  case typeOf x `eqTypeRep` typeRep @b of
    Just HRefl -> Just x
    Nothing -> Nothing
--fmap (\HRefl -> x) (typeOf x `eqTypeRep` typeRep @b)


data Pair where 
  Pair :: (Eq a, Typeable a) => (a, a) -> Pair 

type PolyMap = [Pair]

empty :: PolyMap 
empty = [] 

insert :: (Eq a, Typeable a) => a -> a -> PolyMap -> PolyMap 
insert key value map = 
  Pair (key, value) : map

lookup :: (Eq a, Typeable a) => a -> PolyMap -> Maybe a
lookup _ [] = Nothing 
lookup key (Pair (x, y) : map) = 
  case typeOf key `eqTypeRep` typeOf x of 
    Just HRefl | key == x -> Just y 
    _ -> Some.lookup key map

{-
data (:~~:) (a :: Type) (b :: Type) where
  HRefl :: c :~~: c
-}
