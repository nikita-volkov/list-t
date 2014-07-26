module Stream where

import Stream.Prelude hiding (toList, yield, fold)


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

instance Functor m => Functor (Stream m) where
  fmap f s =
    Stream $ 
      (fmap . fmap) (\(a, b) -> (f a, fmap f b)) $ uncons s

instance (Monad m, Functor m) => Applicative (Stream m) where
  pure = 
    return
  (<*>) = 
    ap


uncons :: Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream m) = 
  m

cons :: Monad m => a -> Stream m a -> Stream m a
cons h t =
  Stream $ return (Just (h, t))
