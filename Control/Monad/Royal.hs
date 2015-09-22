-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Royal
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  royal
-- Portability :  royal
--
-- Introducing his majesty the Royal Monad
--
-- Royal Monads (or relative polymonads) generalize monads, polymonads _and_ relative monads.
--
-- Therefore Royal Monads (capitalize to emphasise royalness) are the king of monads! 
-- 
-- The ultimate abstraction! 
-- 
-- Ditch your boring old relative, poly or ordinary monad, and start programming with the king of monads! 
-- 
-- Bow before the ultimate generality of the Royal Monad, infidels!

{-# LANGUAGE UndecidableInstances,FlexibleInstances,MultiParamTypeClasses #-}
module Control.Monad.Royal where


class RoyalReturn m r where
  royalReturn :: r a -> m a

{- |
 laws:
@ 
  royalBind m royalReturn = m
  royalBind (royalReturn x) f = f x 
  royalBind m (\x -> royalBind (f x) g) = royalBind (royalBind m f) g
@
-}
class (RoyalReturn m r, RoyalReturn n r, RoyalReturn p r) => 
     RoyalMonad m n p r where
  royalBind   :: m a -> (r a -> n a) -> p a




{- | Relative monads
 laws:
@ 
relativeBind m royalReturn = m
relativeBind (royalReturn x) f = f x 
relativeBind m (\x -> relativeBind (f x) g) = relativeBind (relativeBind m f) g
@
-}
class RoyalReturn m r => RelMonad m r where
  relativeBind :: m a -> (r a -> m b) -> m b

class NonRoyalReturn m where
  rreturn :: a -> m a


{- | Relative monads
 laws:
@ 
polyBind m rreturn = m
polyBind (rreturn x) f = f x 
polyBind m (\x -> polyBind (f x) g) = polyBind (polyBind m f) g
@
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
