{-# LANGUAGE UndecidableInstances, CPP #-}
module ListT
(
  ListT,
  -- * Classes
  MonadTransUncons(..),
  MonadCons(..),
  -- * Execution utilities
  head,
  tail,
  null,
  fold,
  foldMaybe,
  toList,
  toReverseList,
  traverse_,
  splitAt,
  -- * Construction utilities
  fromFoldable,
  fromMVar,
  unfold,
  unfoldM,
  repeat,
  -- * Transformation utilities
  -- | 
  -- These utilities only accumulate the transformations
  -- without actually traversing the stream.
  -- They only get applied with a single traversal, 
  -- which happens at the execution.
  Transformation,
  traverse,
  take,
  drop,
  slice,
  -- * Positive numbers
  Positive,
  positive,
)
where

import BasePrelude hiding (uncons, toList, yield, fold, traverse, head, tail, take, drop, repeat, null, traverse_, splitAt)
import Control.Monad.Morph hiding (MonadTrans(..))
import Control.Monad.IO.Class
import Control.Monad.Error.Class 
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Control hiding (embed, embed_)
import Control.Monad.Base

-- |
-- A proper implementation of the list monad-transformer.
-- Useful for streaming of monadic data structures.
-- 
-- Since it has instances of 'MonadPlus' and 'Alternative',
-- you can use general utilities packages like
-- <http://hackage.haskell.org/package/monadplus "monadplus">
-- with it.
newtype ListT m a =
  ListT { unListT :: m (Maybe (a, ListT m a)) }

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
  fmap f =
    ListT . (fmap . fmap) (f *** fmap f) . unListT

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
  hoist f =
    ListT . f . (fmap . fmap) (id *** hoist f) . unListT

instance MMonad ListT where
  embed f (ListT m) =
    f m >>= \case
      Nothing -> mzero
      Just (h, t) -> ListT $ return $ Just $ (h, embed f t)

instance MonadBase b m => MonadBase b (ListT m) where
  liftBase =
    lift . liftBase

#if MIN_VERSION_monad_control(1,0,0)
instance MonadBaseControl b m => MonadBaseControl b (ListT m) where
  type StM (ListT m) a =
    StM m (Maybe (a, ListT m a))
  liftBaseWith runToBase =
    lift $ liftBaseWith $ \runInner -> 
      runToBase $ runInner . uncons
  restoreM inner =
    lift (restoreM inner) >>= \case
      Nothing -> mzero
      Just (h, t) -> cons h t
#else
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
#endif

instance MonadError e m => MonadError e (ListT m) where
  throwError = ListT . throwError
  catchError m handler = ListT $ catchError (unListT m) $ unListT . handler

-- * Classes
-------------------------

-- |
-- A monad transformer capable of deconstructing like a list.
class MonadTrans t => MonadTransUncons t where
  -- |
  -- Execute in the inner monad,
  -- getting the head and the tail.
  -- Returns nothing if it's empty.
  uncons :: Monad m => t m a -> m (Maybe (a, t m a))

instance MonadTransUncons ListT where
  {-# INLINE uncons #-}
  uncons (ListT m) = m


-- |
-- A monad capable of constructing like a list.
class MonadPlus m => MonadCons m where
  -- |
  -- Prepend an element.
  cons :: a -> m a -> m a

instance MonadCons [] where
  cons a m = a : m

instance Monad m => MonadCons (ListT m) where
  {-# INLINABLE cons #-}
  cons h t = ListT $ return (Just (h, t))

instance MonadCons m => MonadCons (ReaderT e m) where
  cons a m = ReaderT $ cons a . runReaderT m


-- * Execution in the inner monad
-------------------------

-- |
-- Execute, getting the head. Returns nothing if it's empty.
{-# INLINABLE head #-}
head :: (Monad m, MonadTransUncons t) => t m a -> m (Maybe a)
head =
  liftM (fmap fst) . uncons

-- |
-- Execute, getting the tail. Returns nothing if it's empty.
{-# INLINABLE tail #-}
tail :: (Monad m, MonadTransUncons t) => t m a -> m (Maybe (t m a))
tail =
  liftM (fmap snd) . uncons

-- |
-- Execute, checking whether it's empty.
{-# INLINABLE null #-}
null :: (Monad m, MonadTransUncons t) => t m a -> m Bool
null =
  liftM (maybe True (const False)) . uncons

-- |
-- Execute, applying a left fold.
{-# INLINABLE fold #-}
fold :: (Monad m, MonadTransUncons t) => (r -> a -> m r) -> r -> t m a -> m r
fold s r = 
  uncons >=> maybe (return r) (\(h, t) -> s r h >>= \r' -> fold s r' t)

-- |
-- A version of 'fold', which allows early termination.
{-# INLINABLE foldMaybe #-}
foldMaybe :: (Monad m, MonadTransUncons t) => (r -> a -> m (Maybe r)) -> r -> t m a -> m r
foldMaybe s r l =
  liftM (maybe r id) $ runMaybeT $ do
    (h, t) <- MaybeT $ uncons l
    r' <- MaybeT $ s r h
    lift $ foldMaybe s r' t

-- |
-- Execute, folding to a list.
{-# INLINABLE toList #-}
toList :: (Monad m, MonadTransUncons t) => t m a -> m [a]
toList =
  liftM ($ []) . fold (\f e -> return $ f . (e :)) id

-- |
-- Execute, folding to a list in a reverse order.
-- Performs more efficiently than 'toList'.
{-# INLINABLE toReverseList #-}
toReverseList :: (Monad m, MonadTransUncons t) => t m a -> m [a]
toReverseList =
  ListT.fold (\l -> return . (:l)) []

-- |
-- Execute, traversing the stream with a side effect in the inner monad. 
{-# INLINABLE traverse_ #-}
traverse_ :: (Monad m, MonadTransUncons t) => (a -> m ()) -> t m a -> m ()
traverse_ f =
  fold (const f) ()

-- |
-- Execute, consuming a list of the specified length and returning the remainder stream.
{-# INLINABLE splitAt #-}
splitAt :: (Monad m, MonadTransUncons t, MonadPlus (t m)) => Int -> t m a -> m ([a], t m a)
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
fromFoldable :: (MonadCons m, Foldable f) => f a -> m a
fromFoldable = 
  foldr cons mzero

-- |
-- Construct from an MVar, interpreting a value of Nothing as an end.
fromMVar :: (MonadCons m, MonadIO m) => MVar (Maybe a) -> m a
fromMVar v =
  fix $ \loop -> liftIO (takeMVar v) >>= maybe mzero (flip cons loop)

-- |
-- Construct by unfolding a pure data structure.
{-# INLINABLE unfold #-}
unfold :: (MonadCons m) => (b -> Maybe (a, b)) -> b -> m a
unfold f s =
  maybe mzero (\(h, t) -> cons h (unfold f t)) (f s)

-- |
-- Construct by unfolding a monadic data structure
--
-- This is the most memory-efficient way to construct a ListT where
-- the length depends on the inner monad.
{-# INLINABLE unfoldM #-}
unfoldM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldM f = go where
  go s = ListT $ f s >>= \case
    Nothing -> return Nothing
    Just (a,r) -> return (Just (a, go r))

-- |
-- Produce an infinite stream.
{-# INLINABLE repeat #-}
repeat :: (MonadCons m) => a -> m a
repeat = 
  fix . cons


-- * Transformation
-------------------------

-- |
-- A function, which updates the contents of a list transformer.
-- 
-- Since it's merely just a function,
-- you can run it by passing a list transformer as an argument.
type Transformation m a b = 
  forall t. (Monad m, MonadCons (t m), MonadTransUncons t) =>
  t m a -> t m b

-- |
-- A transformation,
-- which traverses the stream with an action in the inner monad.
{-# INLINABLE traverse #-}
traverse :: (a -> m b) -> Transformation m a b
traverse f s =
  lift (uncons s) >>= 
  mapM (\(h, t) -> lift (f h) >>= \h' -> cons h' (traverse f t)) >>=
  maybe mzero return

-- |
-- A transformation,
-- reproducing the behaviour of @Data.List.'Data.List.take'@.
{-# INLINABLE take #-}
take :: Int -> Transformation m a a
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
-- A transformation,
-- reproducing the behaviour of @Data.List.'Data.List.drop'@.
{-# INLINABLE drop #-}
drop :: Int -> Transformation m a a
drop =
  \case
    n | n > 0 ->
      lift . uncons >=> maybe mzero (drop (pred n) . snd)
    _ ->
      id

-- |
-- A transformation,
-- which slices a list into chunks of the specified length.
{-# INLINABLE slice #-}
slice :: Positive Int -> Transformation m a [a]
slice n l = 
  do
    (h, t) <- lift $ splitAt (case n of Positive n -> n) l
    case h of
      [] -> mzero
      _ -> cons h (slice n t)


-- * Positive numbers
-------------------------

-- |
-- A newtype wrapper around a number,
-- which ensures that it is greater than zero.
newtype Positive n = 
  Positive n
  deriving (Show, Read, Eq, Ord, Typeable, Data, Generic)

-- |
-- A smart constructor for positive numbers.
positive :: (Ord n, Num n) => n -> Maybe (Positive n)
positive =
  \case
    n | n > 0 -> Just $ Positive n
    _ -> Nothing

