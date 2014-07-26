{-# OPTIONS_GHC -F -pgmF htfpp #-}

import BasePrelude
import Test.Framework
import Development.Placeholders
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import qualified Stream as S


main = htfMain $ htf_thisModulesTests


test_monadLaw1 =
  assertBool =<< streamsEqual (return a >>= k) (k a)
  where
    a = 2
    k a = return $ chr a

test_monadLaw2 =
  assertBool =<< streamsEqual (m >>= return) m
  where
    m = S.fromList ['a'..'z']

test_monadLaw3 =
  assertBool =<< streamsEqual (m >>= (\x -> k x >>= h)) ((m >>= k) >>= h)
  where
    m = S.fromList ['a'..'z']
    k a = return $ ord a
    h a = return $ a + 1

test_monadLaw4 =
  assertBool =<< streamsEqual (fmap f xs) (xs >>= return . f)
  where
    f = ord
    xs = S.fromList ['a'..'z']

test_mappend =
  assertBool =<< 
    streamsEqual 
      (S.fromList [0..7]) 
      (S.fromList [0..3] <> S.fromList [4..7])

test_mappendAndTake =
  assertBool =<< 
    streamsEqual 
      (S.fromList [0..5]) 
      (S.take 6 $ S.fromList [0..3] <> S.fromList [4..7])

test_traverseDoesntCauseTraversal =
  do
    ref <- newIORef 0
    (flip runReaderT) ref (S.toList stream3)
    assertEqual 3 =<< readIORef ref
  where
    stream1 =
      do
        ref <- lift $ ask
        x <- S.fromList ['a'..'z']
        liftIO $ modifyIORef ref (+1)
        return x
    stream2 =
      S.traverse (return . toUpper) stream1
    stream3 =
      S.take 3 stream2

test_mappendDoesntCauseTraversal =
  do
    ref <- newIORef 0
    (flip runReaderT) ref (S.toList $ S.take 5 $ stream <> stream)
    assertEqual 5 =<< readIORef ref
  where
    stream =
      do
        ref <- lift $ ask
        x <- S.fromList [0..4]
        liftIO $ modifyIORef ref (+1)
        return x

test_takeDoesntCauseTraversal =
  do
    ref <- newIORef 0
    (flip runReaderT) ref (S.toList $ S.take 3 $ S.take 7 $ stream)
    assertEqual 3 =<< readIORef ref
  where
    stream =
      do
        ref <- lift $ ask
        x <- S.fromList [0..10]
        liftIO $ modifyIORef ref (+1)
        return x


streamsEqual :: (Applicative m, Monad m, Eq a) => S.Stream m a -> S.Stream m a -> m Bool
streamsEqual a b =
  (==) <$> S.toList a <*> S.toList b
