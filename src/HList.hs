module HList where

import Data.Kind (Constraint, Type)

import Nat
import Vec


data HList (as :: [Type]) where
  HNil :: HList '[]
  HCons :: a -> HList as -> HList (a ': as)

foo :: HList '[Int, String, Bool]
foo = HCons 10 $ HCons "hello" $ HCons True $ HNil

hlength :: Num n => HList as -> n
hlength = go 0
  where
    go :: Num n => n -> HList bs -> n
    go a HNil = a
    go a (HCons _ xs) = go (a + 1) xs


type family All (c :: k -> Constraint) (as :: [k]) :: Constraint where
  All _ '[] = ()
  All c (a ': as) = (c a, All c as)

instance All Show xs => Show (HList xs) where
  show xs = "[" <> go xs
    where
      go :: All Show ys => HList ys -> String
      go HNil = "]"
      go (HCons y HNil) = show y<>"]"
      go (HCons y ys) = show y<>","<>go ys


type ShowAndNum t = (Show t, Num t)

showZeroAs :: forall t. ShowAndNum t => String
showZeroAs = show (0 :: t)


hhead :: HList (a ': as) -> a
hhead (HCons x _) = x

htail :: HList (a ': as) -> HList as 
htail (HCons _ xs) = xs

type family Reverse (as :: [k]) :: [k] where 
  Reverse xs = ReverseGo xs '[]

type family ReverseGo (as :: [k]) (acc :: [k])  :: [k] where 
  ReverseGo '[] acc = acc 
  ReverseGo (x ': xs) acc = ReverseGo xs (x ': acc) 

hreverse :: HList as -> HList (ReverseGo as '[])
hreverse xs   = go xs HNil where 
  go :: HList as -> HList bs -> HList (ReverseGo as bs)
  go HNil acc = acc 
  go (HCons x xs) acc = go xs (HCons x acc)

type family Replicate (n :: Nat) (a :: k) :: [k] where
  Replicate 'Zero _ = '[]
  Replicate ('Succ n) a = a ': Replicate n a

vec2hlist :: Vec n a -> HList (Replicate n a)
vec2hlist VNil = HNil
vec2hlist (VCons x xs) = HCons x (vec2hlist xs)

vec2list :: Vec n a -> [a]
vec2list VNil = []
vec2list (VCons x xs) = (x : vec2list xs)

type family Len (as :: [k]) :: Nat where
  Len '[] = 'Zero
  Len (x ': xs) = 'Succ (Len xs)

hlist2vec :: All ((~) a) as => HList as -> Vec (Len as) a
hlist2vec HNil = VNil
hlist2vec (HCons x xs) = VCons x (hlist2vec xs)

hlist2list :: All ((~) a) as => HList as -> [a] 
hlist2list HNil = []
hlist2list (HCons x xs) = (x : hlist2list xs)

class HList2Vec' (n :: Nat) where
  hlist2vec' :: HList (Replicate n a) -> Vec n a

instance HList2Vec' 'Zero where
  hlist2vec' HNil = VNil

instance HList2Vec' n => HList2Vec' ('Succ n) where
  hlist2vec' (HCons x xs) = VCons x (hlist2vec' xs)


type family Map (f :: Type) (as :: [k1]) :: [k2] where
  Map _ '[] = '[]
  Map f (a ': as) = MapType f a ': Map f as



-- This is stupid, I don't like it
class List2Vec (n :: Nat) where 
  list2vec :: [a] -> Vec n a

instance List2Vec 'Zero where 
  list2vec [] = VNil 

instance List2Vec n => List2Vec ('Succ n) where 
  list2vec (x : xs) = VCons x (list2vec xs)
  list2vec _ = error "list2vec expects more elements than provided"

class List2HList (n :: Nat) where 
  list2hlist :: [a] -> HList (Replicate n a)
  
instance List2HList 'Zero where 
  list2hlist _ = HNil 

instance List2HList n => List2HList ('Succ n) where 
  list2hlist (x:xs) = HCons x (list2hlist @n xs)
  list2hlist _ = error "list2vec expects more elements than provided"


-- list2hlist = vec2hlist . list2vec

data Shower

class MapFunction (f :: Type) (a :: Type) where
  type MapType f a :: Type
  mapVal :: a -> MapType f a

instance MapFunction Shower Int where
  type MapType Shower Int = String
  mapVal = show

instance MapFunction Shower String where
  type MapType Shower String = String
  mapVal = id

instance MapFunction Shower Bool where
  type MapType Shower Bool = Char
  mapVal False = 'N'
  mapVal True = 'Y'


class HMap (f :: Type) (as :: [Type]) where
  hmap :: HList as -> HList (Map f as)

instance HMap f '[] where
  hmap HNil = HNil

instance (MapFunction f a, HMap f as) => HMap f (a ': as) where
  hmap (HCons x xs) = HCons (mapVal @f x) (hmap @f xs)
