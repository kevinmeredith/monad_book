{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

import Data.IORef

f :: a ~ b => a -> b -> b
f a b = a

g :: a -> a -> a
g a b = a

class IOStore store where
	newIO :: a -> IO (store a)

instance IOStore Maybe where 
	newIO _ = return Nothing

class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)

instance Store IORef where
  type StoreMonad IORef = IO
  new = newIORef

data Defined = Yes | No

class MyEq a where 
	eq :: a -> a -> Bool

type family IsEq (a :: *) :: Defined	