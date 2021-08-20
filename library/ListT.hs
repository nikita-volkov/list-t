{-# LANGUAGE UndecidableInstances, CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ListT
(
  ListT(..),
  -- * Execution utilities
  uncons,
  head,
  tail,
  null,
  fold,
  foldMaybe,
  applyFoldM,
  toList,
  toReverseList,
  traverse_,
  splitAt,
  -- * Construction utilities
  cons,
  fromFoldable,
  fromMVar,
  unfold,
  unfoldM,
  repeat,
  -- * Transformation utilities
  -- | 
  -- These utilities only accumulate the transformations
  -- without actually traversing the stream.
  -- They only get applied in a single traversal, 
  -- which only happens at the execution.
  traverse,
  take,
  drop,
  slice,
  fromListT,
  fromListTWith
)
where

import ListT.Prelude hiding (uncons, toList, yield, fold, traverse, head, tail, take, drop, repeat, null, traverse_, splitAt)
import Control.Monad

-- |
-- A proper implementation of the list monad-transformer.
-- Useful for streaming of monadic data structures.
-- 
-- Since it has instances of 'MonadPlus' and 'Alternative',
-- you can use general utilities packages like
-- <http://hackage.haskell.org/package/monadplus "monadplus">
-- with it.
newtype ListT m a =
  ListT (m (Maybe (a, ListT m a)))
  deriving (Foldable, Traversable, Generic)

deriving instance Show (m (Maybe (a, ListT m a))) => Show (ListT m a)
deriving instance Read (m (Maybe (a, ListT m a))) => Read (ListT m a)
deriving instance Eq (m (Maybe (a, ListT m a))) => Eq (ListT m a)
deriving instance Ord (m (Maybe (a, ListT m a))) => Ord (ListT m a)
deriving instance (Typeable m, Typeable a, Data (m (Maybe (a, ListT m a)))) => Data (ListT m a)

instance Eq1 m => Eq1 (ListT m) where
  liftEq eq = go
    where
      go (ListT m) (ListT n) = liftEq (liftEq (\(a, as) (b, bs) -> eq a b && go as bs)) m n

instance Ord1 m => Ord1 (ListT m) where
  liftCompare cmp = go
    where
      go (ListT m) (ListT n) = liftCompare (liftCompare (\(a, as) (b, bs) -> cmp a b <> go as bs)) m n

instance Show1 m => Show1 (ListT m) where
  -- I wish I were joking.
  liftShowsPrec sp (sl :: [a] -> ShowS) = mark
    where
      bob :: Int -> m (Maybe (a, ListT m a)) -> ShowS
      bob = liftShowsPrec jill edith

      edith :: [Maybe (a, ListT m a)] -> ShowS
      edith = liftShowList jack martha

      jill :: Int -> Maybe (a, ListT m a) -> ShowS
      jill = liftShowsPrec jack martha

      martha :: [(a, ListT m a)] -> ShowS
      martha = liftShowList2 sp sl mark juan

      mark :: Int -> ListT m a -> ShowS
      mark d (ListT m) = showsUnaryWith bob "ListT" d m

      juan :: [ListT m a] -> ShowS
      juan = liftShowList sp sl

      jack :: Int -> (a, ListT m a) -> ShowS
      jack = liftShowsPrec2 sp sl mark juan

instance Monad m => Semigroup (ListT m a) where
  (<>) (ListT m1) (ListT m2) =
    ListT $
      m1 >>=
        \case
          Nothing ->
            m2
          Just (h1, s1') ->
            return (Just (h1, ((<>) s1' (ListT m2))))

instance Monad m => Monoid (ListT m a) where
  mempty =
    ListT $ 
      return Nothing
  mappend = (<>)

instance Functor m => Functor (ListT m) where
  fmap f =
    ListT . (fmap . fmap) (f *** fmap f) . uncons

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
#if !MIN_VERSION_base(4,11,0)
  fail _ =
    mempty
#endif

instance Monad m => MonadFail (ListT m) where
  fail _ =
    inline mempty

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
    ListT . f . (liftM . fmap) (id *** hoist f) . uncons

instance MMonad ListT where
  embed f (ListT m) =
    f m >>= \case
      Nothing -> mzero
      Just (h, t) -> ListT $ return $ Just $ (h, embed f t)

instance MonadBase b m => MonadBase b (ListT m) where
  liftBase =
    lift . liftBase

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

instance MonadError e m => MonadError e (ListT m) where
  throwError = ListT . throwError
  catchError m handler = ListT $ catchError (uncons m) $ uncons . handler

instance Monad m => MonadLogic (ListT m) where
  msplit (ListT m) = lift m

  -- The below are copied from the defaults currently in
  -- the logict master branch. The ones on Hackage have some
  -- extra binds going on.
  interleave m1 m2 = msplit m1 >>=
                      maybe m2 (\(a, m1') -> pure a <|> interleave m2 m1')

  m >>- f = msplit m >>= maybe empty
    (\(a, m') -> interleave (f a) (m' >>- f))

  ifte t th el = msplit t >>= maybe el (\(a,m) -> th a <|> (m >>= th))

  once m = msplit m >>= maybe empty (\(a, _) -> pure a)

  lnot m = msplit m >>= maybe (pure ()) (const empty)

-- | Convert a 'ListT' to a similar monad transformer, such as
-- @LogicT@.
fromListT :: (Monad m, Monad (t m), Alternative (t m), MonadTrans t) => ListT m a -> t m a
fromListT = fromListTWith lift

-- | Use a monad morphism to convert a 'ListT' to a similar
-- monad, such as '[]'.
fromListTWith :: (Monad n, Alternative n) => (forall a. m a -> n a) -> ListT m a -> n a
fromListTWith f = go
  where
    go (ListT m) = f m >>= \case
      Nothing -> empty
      Just (a, as) -> pure a <|> go as

-- * Execution in the inner monad
-------------------------

-- |
-- Execute in the inner monad,
-- getting the head and the tail.
-- Returns nothing if it's empty.
uncons :: ListT m a -> m (Maybe (a, ListT m a))
uncons (ListT m) =
  m

-- |
-- Execute, getting the head. Returns nothing if it's empty.
{-# INLINABLE head #-}
head :: Monad m => ListT m a -> m (Maybe a)
head =
  liftM (fmap fst) . uncons

-- |
-- Execute, getting the tail. Returns nothing if it's empty.
{-# INLINABLE tail #-}
tail :: Monad m => ListT m a -> m (Maybe (ListT m a))
tail =
  liftM (fmap snd) . uncons

-- |
-- Execute, checking whether it's empty.
{-# INLINABLE null #-}
null :: Monad m => ListT m a -> m Bool
null =
  liftM (maybe True (const False)) . uncons

-- |
-- Execute, applying a left fold.
{-# INLINABLE fold #-}
fold :: Monad m => (r -> a -> m r) -> r -> ListT m a -> m r
fold s r = 
  uncons >=> maybe (return r) (\(h, t) -> s r h >>= \r' -> fold s r' t)

-- |
-- A version of 'fold', which allows early termination.
{-# INLINABLE foldMaybe #-}
foldMaybe :: Monad m => (r -> a -> m (Maybe r)) -> r -> ListT m a -> m r
foldMaybe s r l =
  liftM (maybe r id) $ runMaybeT $ do
    (h, t) <- MaybeT $ uncons l
    r' <- MaybeT $ s r h
    lift $ foldMaybe s r' t

-- |
-- Apply a left fold abstraction from the \"foldl\" package.
applyFoldM :: Monad m => FoldM m i o -> ListT m i -> m o
applyFoldM (FoldM step init extract) lt = do
  a <- init
  b <- fold step a lt
  extract b

-- |
-- Execute, folding to a list.
{-# INLINABLE toList #-}
toList :: Monad m => ListT m a -> m [a]
toList =
  liftM ($ []) . fold (\f e -> return $ f . (e :)) id

-- |
-- Execute, folding to a list in the reverse order.
-- Performs more efficiently than 'toList'.
{-# INLINABLE toReverseList #-}
toReverseList :: Monad m => ListT m a -> m [a]
toReverseList =
  ListT.fold (\l -> return . (:l)) []

-- |
-- Execute, traversing the stream with a side effect in the inner monad. 
{-# INLINABLE traverse_ #-}
traverse_ :: Monad m => (a -> m ()) -> ListT m a -> m ()
traverse_ f =
  fold (const f) ()

-- |
-- Execute, consuming a list of the specified length and returning the remainder stream.
{-# INLINABLE splitAt #-}
splitAt :: Monad m => Int -> ListT m a -> m ([a], ListT m a)
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
-- Prepend an element.
cons :: Monad m => a -> ListT m a -> ListT m a
cons h t =
  ListT $ return (Just (h, t))

-- |
-- Construct from any foldable.
{-# INLINABLE fromFoldable #-}
fromFoldable :: (Monad m, Foldable f) => f a -> ListT m a
fromFoldable = 
  foldr cons mzero

-- |
-- Construct from an MVar, interpreting the value of Nothing as the end.
fromMVar :: (MonadIO m) => MVar (Maybe a) -> ListT m a
fromMVar v =
  fix $ \loop -> liftIO (takeMVar v) >>= maybe mzero (flip cons loop)

-- |
-- Construct by unfolding a pure data structure.
{-# INLINABLE unfold #-}
unfold :: Monad m => (b -> Maybe (a, b)) -> b -> ListT m a
unfold f s =
  maybe mzero (\(h, t) -> cons h (unfold f t)) (f s)

-- |
-- Construct by unfolding a monadic data structure
--
-- This is the most memory-efficient way to construct ListT where
-- the length depends on the inner monad.
{-# INLINABLE unfoldM #-}
unfoldM :: Monad m => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldM f = go where
  go s = ListT $ f s >>= \case
    Nothing -> return Nothing
    Just (a,r) -> return (Just (a, go r))

-- |
-- Produce an infinite stream.
{-# INLINABLE repeat #-}
repeat :: Monad m => a -> ListT m a
repeat = 
  fix . cons


-- * Transformation
-------------------------

-- |
-- A transformation,
-- which traverses the stream with an action in the inner monad.
{-# INLINABLE traverse #-}
traverse :: Monad m => (a -> m b) -> ListT m a -> ListT m b
traverse f s =
  lift (uncons s) >>= 
  mapM (\(h, t) -> lift (f h) >>= \h' -> cons h' (traverse f t)) >>=
  maybe mzero return

-- |
-- A transformation,
-- reproducing the behaviour of @Data.List.'Data.List.take'@.
{-# INLINABLE take #-}
take :: Monad m => Int -> ListT m a -> ListT m a
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
drop :: Monad m => Int -> ListT m a -> ListT m a
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
slice :: Monad m => Int -> ListT m a -> ListT m [a]
slice n l = 
  do
    (h, t) <- lift $ splitAt n l
    case h of
      [] -> mzero
      _ -> cons h (slice n t)

