{-# OPTIONS_GHC -Wno-dodgy-imports #-}

module ListT.Prelude
  ( module Exports,
    bimapPair',
    secondPair',
  )
where

import Control.Applicative as Exports
import Control.Category as Exports
import Control.Concurrent as Exports
import Control.Exception as Exports
import Control.Foldl as Exports (Fold (..), FoldM (..))
import Control.Monad as Exports hiding (fail, forM, forM_, mapM, mapM_, msum, sequence, sequence_)
import Control.Monad.Base as Exports
import Control.Monad.Error.Class as Exports
import Control.Monad.Fail as Exports
import Control.Monad.Fix as Exports hiding (fix)
import Control.Monad.IO.Class as Exports
import Control.Monad.Logic.Class as Exports
import Control.Monad.Morph as Exports hiding (MonadTrans (..))
import Control.Monad.Reader.Class as Exports
import Control.Monad.ST as Exports
import Control.Monad.State.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Control as Exports hiding (embed, embed_)
import Control.Monad.Trans.Maybe as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Zip as Exports
import Data.Bits as Exports
import Data.Bool as Exports
import Data.Char as Exports
import Data.Coerce as Exports
import Data.Complex as Exports
import Data.Data as Exports
import Data.Dynamic as Exports
import Data.Either as Exports
import Data.Fixed as Exports
import Data.Foldable as Exports
import Data.Function as Exports hiding (id, (.))
import Data.Functor as Exports hiding (unzip)
import Data.Functor.Classes as Exports
import Data.IORef as Exports
import Data.Int as Exports
import Data.Ix as Exports
import Data.List as Exports hiding (all, and, any, concat, concatMap, elem, find, foldl, foldl', foldl1, foldr, foldr1, isSubsequenceOf, mapAccumL, mapAccumR, maximum, maximumBy, minimum, minimumBy, notElem, or, product, sortOn, sum, uncons)
import Data.Maybe as Exports
import Data.Monoid as Exports hiding (First, Last, getFirst, getLast, (<>))
import Data.Ord as Exports
import Data.Proxy as Exports
import Data.Ratio as Exports
import Data.STRef as Exports
import Data.Semigroup as Exports
import Data.String as Exports
import Data.Traversable as Exports
import Data.Tuple as Exports
import Data.Unique as Exports
import Data.Version as Exports
import Data.Word as Exports
import Debug.Trace as Exports
import Foreign.ForeignPtr as Exports
import Foreign.Ptr as Exports
import Foreign.StablePtr as Exports
import Foreign.Storable as Exports
import GHC.Conc as Exports hiding (threadWaitRead, threadWaitReadSTM, threadWaitWrite, threadWaitWriteSTM, withMVar)
import GHC.Exts as Exports (groupWith, inline, lazy, sortWith)
import GHC.Generics as Exports (Generic)
import GHC.IO.Exception as Exports
import Numeric as Exports
import System.Environment as Exports
import System.Exit as Exports
import System.IO as Exports (Handle, hClose)
import System.IO.Error as Exports
import System.IO.Unsafe as Exports
import System.Mem as Exports
import System.Mem.StableName as Exports
import System.Timeout as Exports
import Text.Printf as Exports (hPrintf, printf)
import Text.Read as Exports (Read (..), readEither, readMaybe)
import Unsafe.Coerce as Exports
import Prelude as Exports hiding (all, and, any, concat, concatMap, elem, fail, foldl, foldl1, foldr, foldr1, id, mapM, mapM_, maximum, minimum, notElem, or, product, sequence, sequence_, sum, unzip, (.))

-- |
-- A slightly stricter version of Data.Bifunctor.bimap.
-- There's no benefit to producing lazy pairs here.
bimapPair' :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
bimapPair' f g = \(a, c) -> (f a, g c)

-- |
-- A slightly stricter version of Data.Bifunctor.second
-- that doesn't produce gratuitous lazy pairs.
secondPair' :: (b -> c) -> (a, b) -> (a, c)
secondPair' f = \(a, b) -> (a, f b)
