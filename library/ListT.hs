module ListT where

import ListT.Prelude hiding (toList, yield, fold, traverse, head, tail, take)


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
  fmap f s =
    ListT $ 
      (fmap . fmap) (\(a, b) -> (f a, fmap f b)) $ uncons s

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


-- * Execution in the base monad
-------------------------

{-# INLINABLE uncons #-}
uncons :: ListT m a -> m (Maybe (a, ListT m a))
uncons (ListT m) = 
  m

{-# INLINABLE head #-}
head :: Monad m => ListT m a -> m (Maybe a)
head =
  liftM (fmap fst) . uncons

{-# INLINABLE tail #-}
tail :: Monad m => ListT m a -> m (Maybe (ListT m a))
tail =
  liftM (fmap snd) . uncons

{-# INLINABLE null #-}
null :: Monad m => ListT m a -> m Bool
null =
  liftM (maybe True (const False)) . uncons

{-# INLINABLE fold #-}
fold :: Monad m => (r -> a -> m r) -> r -> ListT m a -> m r
fold s r = 
  uncons >=> maybe (return r) (\(h, t) -> s r h >>= \r' -> fold s r' t)

-- |
-- Convert to a list.
{-# INLINABLE toList #-}
toList :: Monad m => ListT m a -> m [a]
toList =
  liftM ($ []) . fold (\f e -> return $ f . (e :)) id

{-# INLINABLE traverse_ #-}
traverse_ :: Monad m => (a -> m ()) -> ListT m a -> m ()
traverse_ f =
  fold (const f) ()

-- * Construction
-------------------------

-- |
-- Construct from any foldable.
{-# INLINABLE fromFoldable #-}
fromFoldable :: (Monad m, Foldable f) => f a -> ListT m a
fromFoldable = 
  foldr cons mempty

-- |
-- Construct by unfolding a pure data structure.
{-# INLINABLE unfold #-}
unfold :: Monad m => (b -> Maybe (a, b)) -> b -> ListT m a
unfold f s =
  maybe mempty (\(h, t) -> cons h (unfold f t)) (f s)


-- * Transformation
-------------------------

{-# INLINABLE cons #-}
cons :: Monad m => a -> ListT m a -> ListT m a
cons h t =
  ListT $ return (Just (h, t))

{-# INLINABLE traverse #-}
traverse :: Monad m => (a -> m b) -> ListT m a -> ListT m b
traverse f s =
  ListT $ 
    uncons s >>= mapM (\(h, t) -> f h >>= \h' -> return (h', traverse f t))

{-# INLINABLE take #-}
take :: Monad m => Int -> ListT m a -> ListT m a
take =
  \case
    n | n > 0 ->
      ListT . (liftM . fmap) (\(h, t) -> (h, take (pred n) t)) . uncons
    _ ->
      mempty
