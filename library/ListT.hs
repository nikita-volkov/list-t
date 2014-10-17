{-# LANGUAGE UndecidableInstances #-}
module ListT
(
  ListT,
  -- * Classes
  ListTrans(..),
  ListMonad(..),
  -- * Execution utilities
  head,
  tail,
  null,
  fold,
  toList,
  traverse_,
  splitAt,
  -- * Construction utilities
  fromFoldable,
  unfold,
  repeat,
  -- * Transformation utilities
  -- | 
  -- These utilities only accumulate the transformations
  -- without actually traversing the stream.
  -- They only get applied with a single traversal, 
  -- which happens at the execution.
  traverse,
  take,
  drop,
)
where

import BasePrelude hiding (toList, yield, fold, traverse, head, tail, take, drop, repeat, null, traverse_, splitAt)
import Control.Monad.Morph
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Control
import Control.Monad.Base

-- |
-- A proper implementation of a list monad-transformer.
-- Useful for streaming of monadic data structures.
-- 
-- Since it has instances of 'MonadPlus' and 'Alternative',
-- you can use general utilities packages like
-- <http://hackage.haskell.org/package/monadplus "monadplus">
-- with it.
newtype ListT m a =
  ListT (m (Maybe (a, ListT m a)))

instance Monad m => Monoid (ListT m a) where
  mempty =
    ListT $ 
      return Nothing
  mappend (ListT m1) (ListT m2) =
    ListT $
      m1 >>=
        \case
          Nothing ->
            m2
          Just (h1, s1') ->
            return (Just (h1, (mappend s1' (ListT m2))))

instance Functor m => Functor (ListT m) where
  fmap f (ListT m) =
    ListT $ (fmap . fmap) (\(a, b) -> (f a, fmap f b)) m

instance (Monad m, Functor m) => Applicative (ListT m) where
  pure = 
    return
  (<*>) = 
    ap

instance (Monad m, Functor m) => Alternative (ListT m) where
  empty = 
    inline mzero
  (<|>) = 
    inline mplus

instance Monad m => Monad (ListT m) where
  return a =
    ListT $ return (Just (a, (ListT (return Nothing))))
  (>>=) s1 k2 =
    ListT $
      uncons s1 >>=
        \case
          Nothing ->
            return Nothing
          Just (h1, t1) ->
            uncons $ k2 h1 <> (t1 >>= k2)

instance Monad m => MonadPlus (ListT m) where
  mzero = 
    inline mempty
  mplus = 
    inline mappend

instance MonadTrans ListT where
  lift =
    ListT . liftM (\a -> Just (a, mempty))

instance MonadIO m => MonadIO (ListT m) where
  liftIO =
    lift . liftIO

instance MFunctor ListT where
  hoist f (ListT m) =
    ListT $ f $ m >>= return . fmap (\(h, t) -> (h, hoist f t))

instance MonadBase b m => MonadBase b (ListT m) where
  liftBase =
    lift . liftBase

instance MonadBaseControl b m => MonadBaseControl b (ListT m) where
  newtype StM (ListT m) a =
    StM (StM m (Maybe (a, ListT m a)))
  liftBaseWith runToBase =
    lift $ liftBaseWith $ \runInner -> 
      runToBase $ liftM StM . runInner . uncons
  restoreM (StM inner) =
    lift (restoreM inner) >>= \case
      Nothing -> mzero
      Just (h, t) -> cons h t


-- * Classes
-------------------------

-- |
-- A monad transformer capable of executing like a list.
class MonadTrans t => ListTrans t where
  -- |
  -- Execute in the inner monad,
  -- getting the head and the tail.
  -- Returns nothing if it's empty.
  uncons :: t m a -> m (Maybe (a, t m a))

instance ListTrans ListT where
  {-# INLINE uncons #-}
  uncons (ListT m) = m


-- |
-- A monad capable of constructing like a list.
class MonadPlus m => ListMonad m where
  -- |
  -- Prepend an element.
  cons :: a -> m a -> m a

instance ListMonad [] where
  cons a m = a : m

instance Monad m => ListMonad (ListT m) where
  {-# INLINABLE cons #-}
  cons h t = ListT $ return (Just (h, t))

instance ListMonad m => ListMonad (ReaderT e m) where
  cons a m = ReaderT $ cons a . runReaderT m


-- * Execution in the inner monad
-------------------------

-- |
-- Execute, getting the head. Returns nothing if it's empty.
{-# INLINABLE head #-}
head :: (Monad m, ListTrans t) => t m a -> m (Maybe a)
head =
  liftM (fmap fst) . uncons

-- |
-- Execute, getting the tail. Returns nothing if it's empty.
{-# INLINABLE tail #-}
tail :: (Monad m, ListTrans t) => t m a -> m (Maybe (t m a))
tail =
  liftM (fmap snd) . uncons

-- |
-- Execute, checking whether it's empty.
{-# INLINABLE null #-}
null :: (Monad m, ListTrans t) => t m a -> m Bool
null =
  liftM (maybe True (const False)) . uncons

-- |
-- Execute, applying a left fold.
{-# INLINABLE fold #-}
fold :: (Monad m, ListTrans t) => (r -> a -> m r) -> r -> t m a -> m r
fold s r = 
  uncons >=> maybe (return r) (\(h, t) -> s r h >>= \r' -> fold s r' t)

-- |
-- Execute, folding to a list.
{-# INLINABLE toList #-}
toList :: (Monad m, ListTrans t) => t m a -> m [a]
toList =
  liftM ($ []) . fold (\f e -> return $ f . (e :)) id

-- |
-- Execute, traversing the stream with a side effect in the inner monad. 
{-# INLINABLE traverse_ #-}
traverse_ :: (Monad m, ListTrans t) => (a -> m ()) -> t m a -> m ()
traverse_ f =
  fold (const f) ()

{-# INLINABLE splitAt #-}
splitAt :: (Monad m, ListTrans t, MonadPlus (t m)) => Int -> t m a -> m ([a], t m a)
splitAt =
  \case
    n | n > 0 -> \l ->
      uncons l >>= \case
        Nothing -> return ([], mzero)
        Just (h, t) -> do
          (r1, r2) <- splitAt (pred n) t
          return (h : r1, r2)
    _ -> \l -> 
      return ([], l)


-- * Construction
-------------------------

-- |
-- Construct from any foldable.
{-# INLINABLE fromFoldable #-}
fromFoldable :: (ListMonad m, Foldable f) => f a -> m a
fromFoldable = 
  foldr cons mzero

-- |
-- Construct by unfolding a pure data structure.
{-# INLINABLE unfold #-}
unfold :: (ListMonad m) => (b -> Maybe (a, b)) -> b -> m a
unfold f s =
  maybe mzero (\(h, t) -> cons h (unfold f t)) (f s)

-- |
-- Produce an infinite stream.
{-# INLINABLE repeat #-}
repeat :: (ListMonad m) => a -> m a
repeat = 
  fix . cons


-- * Transformation
-------------------------

-- |
-- A transformation,
-- which traverses the stream with an action in the inner monad.
{-# INLINABLE traverse #-}
traverse :: (Monad m, ListMonad (t m), ListTrans t) => (a -> m b) -> t m a -> t m b
traverse f s =
  lift (uncons s) >>= 
  mapM (\(h, t) -> lift (f h) >>= \h' -> cons h' (traverse f t)) >>=
  maybe mzero return

-- |
-- A trasformation, 
-- reproducing the behaviour of @Data.List.'Data.List.take'@.
{-# INLINABLE take #-}
take :: (Monad m, ListMonad (t m), ListTrans t) => Int -> t m a -> t m a
take =
  \case
    n | n > 0 -> \t ->
      lift (uncons t) >>= 
        \case
          Nothing -> t
          Just (h, t) -> cons h (take (pred n) t)
    _ ->
      const $ mzero

-- |
-- A trasformation, 
-- reproducing the behaviour of @Data.List.'Data.List.drop'@.
{-# INLINABLE drop #-}
drop :: (Monad m, MonadPlus (t m), ListTrans t) => Int -> t m a -> t m a
drop =
  \case
    n | n > 0 ->
      lift . uncons >=> maybe mzero (drop (pred n) . snd)
    _ ->
      id



