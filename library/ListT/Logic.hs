{-# language RankNTypes #-}

-- | 'ListT' equivalents (or near-equivalents) of the functions
-- and constructors in "Control.Monad.Logic".
module ListT.Logic
  (
  module LogicClass,
  -- * The 'List' monad
  List,
  list,
  runList,
  observe,
  observeMany,
  observeAll,
  -- * The 'ListT' monad transformer
  ListT (..),
  listT,
  runListT,
  observeT,
  observeManyT,
  observeAllT,
  module MonadClass,
  module TransClass,
  ) where
import ListT
import ListT.Prelude
import qualified Control.Monad.Logic.Class as LogicClass
import qualified Control.Monad.Trans.Class as TransClass
import qualified Control.Monad as MonadClass

type List = ListT Identity

list :: (forall r. (a -> r -> r) -> r -> r) -> List a
list f = listT $ \arr r -> Identity $ f (coerce arr) (runIdentity r)

runList :: List a -> (a -> r -> r) -> r -> r
runList l c n = runIdentity $ runListT l (coerce c) (Identity n)

observe :: List a -> Maybe a
observe = runIdentity . observeT

observeMany :: Int -> List a -> [a]
observeMany n = runIdentity . observeManyT n

observeAll :: List a -> [a]
observeAll = runIdentity . observeAllT

-- | Analogous to the 'Control.Monad.Logic.LogicT' constructor.
listT :: Applicative m => (forall r. (a -> m r -> m r) -> m r -> m r) -> ListT m a
listT f = ListT $ f (\a m -> pure (Just (a, ListT m))) (pure Nothing)

runListT :: Monad m => ListT m a -> (a -> m r -> m r) -> m r -> m r
runListT l0 c n = go l0
  where
    go (ListT m) = m >>= \case
      Nothing -> n
      Just (a, as) -> c a (go as)

-- | Extracts the first result from a 'ListT' computation, returning
-- 'Nothing' if there are none. Note: the version in "Control.Monad.Logic"
-- skips the 'Maybe' in favor of a 'Control.Monad.Fail.MonadFail' constraint,
-- which is rather inconvenient.
observeT :: Monad m => ListT m a -> m (Maybe a)
observeT (ListT m) = m >>= \case
  Nothing -> pure Nothing
  Just (a, _) -> pure (Just a)

-- | A traditional name for 'toList'
observeAllT :: Monad m => ListT m a -> m [a]
observeAllT = ListT.toList

-- | Produce the first results of a 'ListT' computation, discarding
-- the rest.
observeManyT :: Monad m => Int -> ListT m a -> m [a]
observeManyT n _ | n <= 0 = pure []
observeManyT n (ListT m) = m >>= \case
  Nothing -> pure []
  Just (a, as) -> (a:) <$> observeManyT (n - 1) as
