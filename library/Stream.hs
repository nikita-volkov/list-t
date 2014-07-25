module Stream where

import Stream.Prelude hiding (toList, yield, fold)


newtype Stream m a =
  Stream (m (Action m a))

data Action m a =
  Stop |
  Continue a (Stream m a)

instance (Monad m) => Monoid (Action m a) where
  mempty =
    Stop
  mappend =
    \case
      Stop ->
        id
      Continue h (Stream mt) ->
        \a -> Continue h (Stream (mt >>= return . flip mappend a))

instance (Monad m) => Monoid (Stream m a) where
  mempty =
    Stream $ return Stop
  mappend (Stream m1) (Stream m2) =
    Stream $
      m1 >>=
        \case
          Stop ->
            m2
          Continue h1 s1' ->
            return (Continue h1 (mappend s1' (Stream m2)))

instance Monad m => Monad (Stream m) where
  return a =
    Stream (return (Continue a (Stream (return Stop))))
  (>>=) (Stream ma1) k2 =
    Stream $
      ma1 >>=
        \case
          Stop ->
            return Stop
          Continue h1 t1 ->
            case k2 h1 <> (t1 >>= k2) of Stream m -> m

uncons :: (Monad m) => Stream m a -> m (Maybe (a, Stream m a))
uncons (Stream m) =
  m >>= return . actionToMaybe

actionToMaybe :: Action m a -> Maybe (a, Stream m a)
actionToMaybe =
  \case
    Stop ->
      Nothing
    Continue h t ->
      Just (h, t)

