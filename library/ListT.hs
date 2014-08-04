module ListT where

import ListT.Prelude hiding (toList, yield, fold, traverse, head, tail, take, repeat)


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


-- * Class
-------------------------

class MonadTrans t => MonadListTrans t where
  uncons :: t m a -> m (Maybe (a, t m a))
  cons :: Monad m => a -> t m a -> t m a

instance MonadListTrans ListT where
  {-# INLINE uncons #-}
  uncons (ListT m) = 
    m
  cons h t = 
    ListT $ return (Just (h, t))


-- * Execution in the base monad
-------------------------

{-# INLINABLE head #-}
head :: (Monad m, MonadListTrans t) => t m a -> m (Maybe a)
head =
  liftM (fmap fst) . uncons

{-# INLINABLE tail #-}
tail :: (Monad m, MonadListTrans t) => t m a -> m (Maybe (t m a))
tail =
  liftM (fmap snd) . uncons

{-# INLINABLE null #-}
null :: (Monad m, MonadListTrans t) => t m a -> m Bool
null =
  liftM (maybe True (const False)) . uncons

{-# INLINABLE fold #-}
fold :: (Monad m, MonadListTrans t) => (r -> a -> m r) -> r -> t m a -> m r
fold s r = 
  uncons >=> maybe (return r) (\(h, t) -> s r h >>= \r' -> fold s r' t)

-- |
-- Convert to a list.
{-# INLINABLE toList #-}
toList :: (Monad m, MonadListTrans t) => t m a -> m [a]
toList =
  liftM ($ []) . fold (\f e -> return $ f . (e :)) id

{-# INLINABLE traverse_ #-}
traverse_ :: (Monad m, MonadListTrans t) => (a -> m ()) -> t m a -> m ()
traverse_ f =
  fold (const f) ()

-- * Construction
-------------------------

-- |
-- Construct from any foldable.
{-# INLINABLE fromFoldable #-}
fromFoldable :: (Monad m, Foldable f, MonadListTrans t, MonadPlus (t m)) => f a -> t m a
fromFoldable = 
  foldr cons mzero

-- |
-- Construct by unfolding a pure data structure.
{-# INLINABLE unfold #-}
unfold :: (Monad m, MonadListTrans t, MonadPlus (t m)) => (b -> Maybe (a, b)) -> b -> t m a
unfold f s =
  maybe mzero (\(h, t) -> cons h (unfold f t)) (f s)

-- |
-- Produce an infinite stream.
{-# INLINABLE repeat #-}
repeat :: (Monad m, MonadListTrans t) => a -> t m a
repeat = fix . cons

-- * Transformation
-------------------------

{-# INLINABLE traverse #-}
traverse :: (Monad m, MonadListTrans t, MonadPlus (t m)) => (a -> m b) -> t m a -> t m b
traverse f s =
  lift (uncons s) >>= 
  mapM (\(h, t) -> lift (f h) >>= \h' -> cons h' (traverse f t)) >>=
  maybe mzero return

{-# INLINABLE take #-}
take :: (Monad m, MonadListTrans t, MonadPlus (t m)) => Int -> t m a -> t m a
take =
  \case
    n | n > 0 -> \t ->
      lift (uncons t) >>= 
        \case
          Nothing -> t
          Just (h, t) -> cons h (take (pred n) t)
    _ ->
      const $ mzero
