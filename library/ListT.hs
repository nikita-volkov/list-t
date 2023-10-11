module ListT
  ( ListT (..),

    -- * Execution utilities
    uncons,
    head,
    tail,
    null,
    alternate,
    alternateHoisting,
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
  )
where

import ListT.Prelude hiding (drop, fold, head, null, repeat, splitAt, tail, take, toList, traverse, traverse_, uncons, yield)

-- |
-- A proper implementation of the list monad-transformer.
-- Useful for streaming of monadic data structures.
--
-- Since it has instances of 'MonadPlus' and 'Alternative',
-- you can use general utilities packages like
-- <http://hackage.haskell.org/package/monadplus "monadplus">
-- with it.
newtype ListT m a
  = ListT (m (Maybe (a, ListT m a)))
  deriving (Foldable, Traversable, Generic)

deriving instance (Show (m (Maybe (a, ListT m a)))) => Show (ListT m a)

deriving instance (Read (m (Maybe (a, ListT m a)))) => Read (ListT m a)

deriving instance (Eq (m (Maybe (a, ListT m a)))) => Eq (ListT m a)

deriving instance (Ord (m (Maybe (a, ListT m a)))) => Ord (ListT m a)

deriving instance (Typeable m, Typeable a, Data (m (Maybe (a, ListT m a)))) => Data (ListT m a)

instance (Eq1 m) => Eq1 (ListT m) where
  liftEq eq = go
    where
      go (ListT m) (ListT n) = liftEq (liftEq (\(a, as) (b, bs) -> eq a b && go as bs)) m n

instance (Ord1 m) => Ord1 (ListT m) where
  liftCompare cmp = go
    where
      go (ListT m) (ListT n) = liftCompare (liftCompare (\(a, as) (b, bs) -> cmp a b <> go as bs)) m n

instance (Show1 m) => Show1 (ListT m) where
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

instance (Monad m) => Semigroup (ListT m a) where
  (<>) (ListT m1) (ListT m2) =
    ListT $
      m1
        >>= \case
          Nothing ->
            m2
          Just (h1, s1') ->
            return (Just (h1, ((<>) s1' (ListT m2))))

instance (Monad m) => Monoid (ListT m a) where
  mempty =
    ListT $
      return Nothing
  mappend = (<>)

instance (Functor m) => Functor (ListT m) where
  fmap f = go
    where
      go =
        ListT . (fmap . fmap) (bimapPair' f go) . uncons

instance (Monad m, Functor m) => Applicative (ListT m) where
  pure a =
    ListT $ return (Just (a, (ListT (return Nothing))))
  (<*>) =
    ap

  -- This is just like liftM2, but it uses fmap over the second
  -- action. liftM2 can't do that, because it has to deal with
  -- the possibility that someone defines liftA2 = liftM2 and
  -- fmap f = (pure f <*>) (leaving (<*>) to the default).
  liftA2 f m1 m2 = do
    x1 <- m1
    fmap (f x1) m2

instance (Monad m, Functor m) => Alternative (ListT m) where
  empty =
    inline mempty
  (<|>) =
    inline mappend

instance (Monad m) => Monad (ListT m) where
  return = pure

  -- We use a go function so GHC can inline k2
  -- if it likes.
  (>>=) s10 k2 = go s10
    where
      go s1 =
        ListT $
          uncons s1
            >>= \case
              Nothing ->
                return Nothing
              Just (h1, t1) ->
                uncons $ k2 h1 <> go t1

instance (Monad m) => MonadFail (ListT m) where
  fail _ =
    inline mempty

instance (Monad m) => MonadPlus (ListT m) where
  mzero =
    inline mempty
  mplus =
    inline mappend

instance MonadTrans ListT where
  lift =
    ListT . fmap (\a -> Just (a, mempty))

instance (MonadIO m) => MonadIO (ListT m) where
  liftIO =
    lift . liftIO

instance MFunctor ListT where
  hoist f = go
    where
      go (ListT run) =
        ListT . f $
          run <&> \case
            Just (elem, next) -> Just (elem, go next)
            Nothing -> Nothing

instance MMonad ListT where
  embed f (ListT m) =
    f m >>= \case
      Nothing -> mzero
      Just (h, t) -> ListT $ return $ Just $ (h, embed f t)

instance (MonadBase b m) => MonadBase b (ListT m) where
  liftBase =
    lift . liftBase

instance (MonadBaseControl b m) => MonadBaseControl b (ListT m) where
  type
    StM (ListT m) a =
      StM m (Maybe (a, ListT m a))
  liftBaseWith runToBase =
    lift $
      liftBaseWith $ \runInner ->
        runToBase $ runInner . uncons
  restoreM inner =
    lift (restoreM inner) >>= \case
      Nothing -> mzero
      Just (h, t) -> cons h t

instance (MonadError e m) => MonadError e (ListT m) where
  throwError = ListT . throwError
  catchError m handler = ListT $ catchError (uncons m) $ uncons . handler

instance (MonadReader e m) => MonadReader e (ListT m) where
  ask = lift ask
  reader = lift . reader
  local r = go
    where
      go (ListT m) = ListT $ local r (fmap (fmap (secondPair' go)) m)

instance (MonadState e m) => MonadState e (ListT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (Monad m) => MonadLogic (ListT m) where
  msplit (ListT m) = lift m

  interleave m1 m2 =
    ListT $
      uncons m1 >>= \case
        Nothing -> uncons m2
        Just (a, m1') -> uncons $ cons a (interleave m2 m1')

  m >>- f =
    ListT $
      uncons m >>= \case
        Nothing -> uncons empty
        Just (a, m') -> uncons $ interleave (f a) (m' >>- f)

  ifte t th el =
    ListT $
      uncons t >>= \case
        Nothing -> uncons el
        Just (a, m) -> uncons $ th a <|> (m >>= th)

  once (ListT m) =
    ListT $
      m >>= \case
        Nothing -> uncons empty
        Just (a, _) -> uncons (return a)

  lnot (ListT m) =
    ListT $
      m >>= \case
        Nothing -> uncons (return ())
        Just _ -> uncons empty

instance (MonadZip m) => MonadZip (ListT m) where
  mzipWith f = go
    where
      go (ListT m1) (ListT m2) =
        ListT $
          mzipWith
            ( mzipWith $
                \(a, as) (b, bs) -> (f a b, go as bs)
            )
            m1
            m2

  munzip (ListT m)
    | (l, r) <- munzip (fmap go m) =
        (ListT l, ListT r)
    where
      go Nothing = (Nothing, Nothing)
      go (Just ((a, b), listab)) =
        (Just (a, la), Just (b, lb))
        where
          -- If the underlying munzip is careful not to leak memory, then we
          -- don't want to defeat it.  We need to be sure that la and lb are
          -- realized as selector thunks.
          {-# NOINLINE remains #-}
          {-# NOINLINE la #-}
          {-# NOINLINE lb #-}
          remains = munzip listab
          (la, lb) = remains

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
{-# INLINEABLE head #-}
head :: (Monad m) => ListT m a -> m (Maybe a)
head =
  fmap (fmap fst) . uncons

-- |
-- Execute, getting the tail. Returns nothing if it's empty.
{-# INLINEABLE tail #-}
tail :: (Monad m) => ListT m a -> m (Maybe (ListT m a))
tail =
  fmap (fmap snd) . uncons

-- |
-- Execute, checking whether it's empty.
{-# INLINEABLE null #-}
null :: (Monad m) => ListT m a -> m Bool
null =
  fmap (maybe True (const False)) . uncons

-- |
-- Execute in the inner monad,
-- using its '(<|>)' function on each entry.
{-# INLINEABLE alternate #-}
alternate :: (Alternative m, Monad m) => ListT m a -> m a
alternate (ListT m) =
  m >>= \case
    Nothing -> empty
    Just (a, as) -> pure a <|> alternate as

-- |
-- Use a monad morphism to convert a 'ListT' to a similar
-- monad, such as '[]'.
--
-- A more efficient alternative to @'alternate' . 'hoist' f@.
{-# INLINEABLE alternateHoisting #-}
alternateHoisting :: (Monad n, Alternative n) => (forall a. m a -> n a) -> ListT m a -> n a
alternateHoisting f = go
  where
    go (ListT m) =
      f m >>= \case
        Nothing -> empty
        Just (a, as) -> pure a <|> go as

-- |
-- Execute, applying a strict left fold.
{-# INLINEABLE fold #-}
fold :: (Monad m) => (b -> a -> m b) -> b -> ListT m a -> m b
fold step = go
  where
    go !acc (ListT run) =
      run >>= \case
        Just (element, next) -> do
          acc' <- step acc element
          go acc' next
        Nothing ->
          return acc

-- |
-- A version of 'fold', which allows early termination.
{-# INLINEABLE foldMaybe #-}
foldMaybe :: (Monad m) => (b -> a -> m (Maybe b)) -> b -> ListT m a -> m b
foldMaybe s r l =
  fmap (maybe r id) $
    runMaybeT $ do
      (h, t) <- MaybeT $ uncons l
      r' <- MaybeT $ s r h
      lift $ foldMaybe s r' t

-- |
-- Apply the left fold abstraction from the \"foldl\" package.
applyFoldM :: (Monad m) => FoldM m i o -> ListT m i -> m o
applyFoldM (FoldM step init extract) lt = do
  a <- init
  b <- fold step a lt
  extract b

-- |
-- Execute, folding to a list.
{-# INLINEABLE toList #-}
toList :: (Monad m) => ListT m a -> m [a]
toList =
  fmap reverse . toReverseList

-- |
-- Execute, folding to a list in the reverse order.
-- Performs more efficiently than 'toList'.
{-# INLINEABLE toReverseList #-}
toReverseList :: (Monad m) => ListT m a -> m [a]
toReverseList =
  fold (\list element -> return (element : list)) []

-- |
-- Execute, traversing the stream with a side effect in the inner monad.
{-# INLINEABLE traverse_ #-}
traverse_ :: (Monad m) => (a -> m ()) -> ListT m a -> m ()
traverse_ f =
  fold (const f) ()

-- |
-- Execute, consuming a list of the specified length and returning the remainder stream.
{-# INLINEABLE splitAt #-}
splitAt :: (Monad m) => Int -> ListT m a -> m ([a], ListT m a)
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
cons :: (Monad m) => a -> ListT m a -> ListT m a
cons h t =
  ListT $ return (Just (h, t))

-- |
-- Construct from any foldable.
{-# INLINEABLE fromFoldable #-}
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
{-# INLINEABLE unfold #-}
unfold :: (Monad m) => (b -> Maybe (a, b)) -> b -> ListT m a
unfold f s =
  maybe mzero (\(h, t) -> cons h (unfold f t)) (f s)

-- |
-- Construct by unfolding a monadic data structure
--
-- This is the most memory-efficient way to construct ListT where
-- the length depends on the inner monad.
{-# INLINEABLE unfoldM #-}
unfoldM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> ListT m a
unfoldM f = go
  where
    go s =
      ListT $
        f s >>= \case
          Nothing -> return Nothing
          Just (a, r) -> return (Just (a, go r))

-- |
-- Produce an infinite stream.
{-# INLINEABLE repeat #-}
repeat :: (Monad m) => a -> ListT m a
repeat =
  fix . cons

-- * Transformation

-------------------------

-- |
-- A transformation,
-- which traverses the stream with an action in the inner monad.
{-# INLINEABLE traverse #-}
traverse :: (Monad m) => (a -> m b) -> ListT m a -> ListT m b
traverse f =
  go
  where
    go (ListT run) =
      ListT $
        run >>= \case
          Nothing -> return Nothing
          Just (a, next) -> f a <&> \b -> Just (b, go next)

-- |
-- A transformation,
-- reproducing the behaviour of @Data.List.'Data.List.take'@.
{-# INLINEABLE take #-}
take :: (Monad m) => Int -> ListT m a -> ListT m a
take =
  \case
    n | n > 0 -> \t ->
      lift (uncons t)
        >>= \case
          Nothing -> t
          Just (h, t) -> cons h (take (pred n) t)
    _ ->
      const $ mzero

-- |
-- A transformation,
-- reproducing the behaviour of @Data.List.'Data.List.drop'@.
{-# INLINEABLE drop #-}
drop :: (Monad m) => Int -> ListT m a -> ListT m a
drop =
  \case
    n
      | n > 0 ->
          lift . uncons >=> maybe mzero (drop (pred n) . snd)
    _ ->
      id

-- |
-- A transformation,
-- which slices a list into chunks of the specified length.
{-# INLINEABLE slice #-}
slice :: (Monad m) => Int -> ListT m a -> ListT m [a]
slice n l =
  do
    (h, t) <- lift $ splitAt n l
    case h of
      [] -> mzero
      _ -> cons h (slice n t)
