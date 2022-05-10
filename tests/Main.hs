{-# OPTIONS_GHC -F -pgmF htfpp #-}

import BasePrelude hiding (toList)
import Control.Monad.Morph
import qualified ListT as L
import MTLPrelude
import Test.Framework

main = htfMain $ htf_thisModulesTests

-- * MMonad

-- embed lift = id
prop_mmonadLaw1 (l :: [Int]) =
  let s = L.fromFoldable l
   in runIdentity $ streamsEqual s (embed lift s)

-- embed f (lift m) = f m
prop_mmonadLaw2 l =
  let s = (L.fromFoldable :: [Int] -> L.ListT Identity Int) l
      f = MaybeT . fmap Just
      run = runIdentity . L.toList . runMaybeT
   in run (f s)
        == run (embed f (lift s))

-- * Applicative

prop_applicativeIdentityLaw (l :: [Int]) =
  runIdentity $ streamsEqual (pure id <*> s) s
  where
    s = L.fromFoldable l

prop_applicativeBehavesLikeList =
  \(ns :: [Int]) ->
    let a = fs <*> ns
        b = runIdentity (toList $ L.fromFoldable fs <*> L.fromFoldable ns)
     in a == b
  where
    fs = [(+ 1), (+ 3), (+ 5)]

-- * Monad

test_monadLaw1 =
  assertBool =<< streamsEqual (return a >>= k) (k a)
  where
    a = 2
    k a = return $ chr a

test_monadLaw2 =
  assertBool =<< streamsEqual (m >>= return) m
  where
    m = L.fromFoldable ['a' .. 'z']

test_monadLaw3 =
  assertBool =<< streamsEqual (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)
  where
    m = L.fromFoldable ['a' .. 'z']
    k a = return $ ord a
    h a = return $ a + 1

test_monadLaw4 =
  assertBool =<< streamsEqual (fmap f xs) (xs >>= return . f)
  where
    f = ord
    xs = L.fromFoldable ['a' .. 'z']

-- * Monoid

test_mappend =
  assertBool
    =<< streamsEqual
      (L.fromFoldable [0 .. 7])
      (L.fromFoldable [0 .. 3] <> L.fromFoldable [4 .. 7])

test_mappendAndTake =
  assertBool
    =<< streamsEqual
      (L.fromFoldable [0 .. 5])
      (L.take 6 $ L.fromFoldable [0 .. 3] <> L.fromFoldable [4 .. 7])

test_mappendDoesntCauseTraversal =
  do
    ref <- newIORef 0
    (flip runReaderT) ref (toList $ L.take 5 $ stream <> stream)
    assertEqual 5 =<< readIORef ref
  where
    stream =
      do
        ref <- lift $ ask
        x <- L.fromFoldable [0 .. 4]
        liftIO $ modifyIORef ref (+ 1)
        return x

-- * Other

test_repeat =
  assertEqual [2, 2, 2] =<< do
    toList $ L.take 3 $ L.repeat (2 :: Int)

test_traverseDoesntCauseTraversal =
  do
    ref <- newIORef 0
    (flip runReaderT) ref (toList stream3)
    assertEqual 3 =<< readIORef ref
  where
    stream1 =
      do
        ref <- lift $ ask
        x <- L.fromFoldable ['a' .. 'z']
        liftIO $ modifyIORef ref (+ 1)
        return x
    stream2 =
      L.traverse (return . toUpper) stream1
    stream3 =
      L.take 3 stream2

test_takeDoesntCauseTraversal =
  do
    ref <- newIORef 0
    (flip runReaderT) ref (toList $ L.take 3 $ L.take 7 $ stream)
    assertEqual 3 =<< readIORef ref
  where
    stream =
      do
        ref <- lift $ ask
        x <- L.fromFoldable [0 .. 10]
        liftIO $ modifyIORef ref (+ 1)
        return x

test_drop =
  assertEqual [3, 4] =<< do
    toList $ L.drop 2 $ L.fromFoldable [1 .. 4]

test_slice =
  assertEqual ["abc", "def", "gh"] =<< do
    toList $ L.slice 3 $ L.fromFoldable ("abcdefgh" :: [Char])

toList :: Monad m => L.ListT m a -> m [a]
toList = L.toList

streamsEqual :: (Applicative m, Monad m, Eq a) => L.ListT m a -> L.ListT m a -> m Bool
streamsEqual a b =
  (==) <$> L.toList a <*> L.toList b
