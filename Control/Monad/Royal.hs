-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Royal
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  royal
-- Portability :  royal
--
-- Introducing his majesty the Royal Monad.
--
-- Royal Monads (or relative polymonads) generalize monads, polymonads _and_ relative monads.
--
-- Therefore Royal Monads (capitalize to emphasise royalness) are the king of monads! 
-- 
-- The ultimate abstraction! 
-- 
-- Ditch your boring old relative, poly or ordinary monad, and start programming with the king of monads! 
-- 
-- Bow before the ultimate generality of the Royal Monad!

{-# LANGUAGE UndecidableInstances,FlexibleInstances,MultiParamTypeClasses #-}
module Control.Monad.Royal where


class RoyalReturn m r where
  royalReturn :: r a -> m a

-- |
-- laws (same as monad, but much more Royal):
--
-- prop> royalBind m royalReturn = m
--
-- prop> royalBind (royalReturn x) f = f x 
--
-- prop> royalBind m (\x -> royalBind (f x) g) = royalBind (royalBind m f) g
--
class (RoyalReturn m r, RoyalReturn n r, RoyalReturn p r) => 
     RoyalMonad m n p r where
  royalBind   :: m a -> (r a -> n b) -> p b




{- | Relative monads
 laws:

prop> relativeBind m royalReturn = m

prop> relativeBind (royalReturn x) f = f x 

prop> relativeBind m (\x -> relativeBind (f x) g) = relativeBind (relativeBind m f) g

-}
class RoyalReturn m r => RelMonad m r where
  relativeBind :: m a -> (r a -> m b) -> m b

class NonRoyalReturn m where
  rreturn :: a -> m a


{- | Relative monads
 laws:

prop> polyBind m rreturn = m

prop> polyBind (rreturn x) f = f x 

prop> polyBind m (\x -> polyBind (f x) g) = polyBind (polyBind m f) g

-}
class (NonRoyalReturn m, NonRoyalReturn n, NonRoyalReturn p) =>
    PolyMonad m n p where
  polyBind :: m a -> (a -> n b) -> p b

newtype Id a = Id { fromId :: a} 

instance NonRoyalReturn m => RoyalReturn m Id where
  royalReturn (Id x) = rreturn x

instance PolyMonad m n p => RoyalMonad m n p Id where
  royalBind m f = polyBind m (f . Id)

instance Monad m => NonRoyalReturn m where
  rreturn = return

instance Monad m => RoyalMonad m m m Id where
  royalBind m f = m >>= (f . Id)
