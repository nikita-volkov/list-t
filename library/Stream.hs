module Stream where

import Stream.Prelude hiding (toList, yield, fold, traverse, head, tail, take)


newtype Stream m a =
  Stream (m (Maybe (a, Stream m a)))

instance Monad m => Monoid (Stream m a) where
  mempty =
    Stream $ 
      return Nothing
  mappend (Stream m1) (Stream m2) =
    Stream $
      m1 >>=
        \case
          Nothing ->
            m2
          Just (h1, s1') ->
            return (Just (h1, (mappend s1' (Stream m2))))

instance Functor m => Functor (Stream m) where
  fmap f s =
    Stream $ 
      (fmap . fmap) (\(a, b) -> (f a, fmap f b)) $ uncons s

instance (Monad m, Functor m) => Applicative (Stream m) where
  pure = 
    return
  (<*>) = 
    ap

instance (Monad m, Functor m) => Alternative (Stream m) where
  empty = inline mzero
  (<|>) = inline mplus

instance Monad m => Monad (Stream m) where
  return a =
    Stream $ return (Just (a, (Stream (return Nothing))))
  (>>=) s1 k2 =
    Stream $
      uncons s1 >>=
        \case
          Nothing ->
            return Nothing
          Just (h1, t1) ->
            uncons $ k2 h1 <> (t1 >>= k2)

instance Monad m => MonadPlus (Stream m) where
  mzero = inline mempty
  mplus = inline mappend


-- * Execution in the base monad
-------------------------

uncons :: Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream m) = 
  m

head :: Monad m => Stream m a -> m (Maybe a)
head =
  liftM (fmap fst) . uncons

tail :: Monad m => Stream m a -> m (Stream m a)
tail =
  liftM (maybe mempty snd) . uncons

null :: Monad m => Stream m a -> m Bool
null =
  liftM (maybe True (const False)) . uncons

fold :: Monad m => (r -> a -> m r) -> r -> Stream m a -> m r
fold s r = 
  uncons >=> maybe (return r) (\(h, t) -> s r h >>= \r' -> fold s r' t)

-- |
-- Convert to a list.
toList :: Monad m => Stream m a -> m [a]
toList =
  liftM ($ []) . fold (\f e -> return $ f . (e :)) id


-- * Construction
-------------------------

-- |
-- Construct from a list.
fromList :: Monad m => [a] -> Stream m a
fromList = 
  unfold unconsList
  where
    unconsList =
      \case
        h : t -> Just (h, t)
        [] -> Nothing

-- |
-- Construct by unfolding a pure data structure.
unfold :: Monad m => (b -> Maybe (a, b)) -> b -> Stream m a
unfold f s =
  maybe mempty (\(h, t) -> cons h (unfold f t)) (f s)


-- * Transformation
-------------------------

cons :: Monad m => a -> Stream m a -> Stream m a
cons h t =
  Stream $ return (Just (h, t))

traverse :: Monad m => (a -> m b) -> Stream m a -> Stream m b
traverse f s =
  Stream $ 
    uncons s >>= mapM (\(h, t) -> f h >>= \h' -> return (h', traverse f t))

take :: Monad m => Int -> Stream m a -> Stream m a
take =
  \case
    n | n > 0 ->
      \s ->
        Stream $
          (liftM . fmap) (\(h, t) -> (h, take (pred n) t)) (uncons s)
    _ ->
      mempty
