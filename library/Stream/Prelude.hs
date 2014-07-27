module Stream.Prelude
( 
  module Exports,
  bug,
  bottom,
)
where

-- base-prelude
-------------------------
import BasePrelude as Exports

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Except as Exports (ExceptT(ExceptT), Except, except, runExcept, runExceptT, mapExcept, mapExceptT, withExcept, withExceptT)
import Control.Monad.Trans.Reader as Exports (Reader, runReader, mapReader, withReader, ReaderT(ReaderT), runReaderT, mapReaderT, withReaderT)
import Control.Monad.Trans.RWS.Strict as Exports (RWS, rws, runRWS, evalRWS, execRWS, mapRWS, withRWS, RWST(RWST), runRWST, evalRWST, execRWST, mapRWST, withRWST)
import Control.Monad.Trans.State.Strict as Exports (State, runState, evalState, execState, mapState, withState, StateT(StateT), runStateT, evalStateT, execStateT, mapStateT, withStateT)
import Control.Monad.Trans.Writer.Strict as Exports (Writer, runWriter, execWriter, mapWriter, WriterT(..), execWriterT, mapWriterT)
import Data.Functor.Identity as Exports

-- mmorph
-------------------------
import Control.Monad.Morph as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"stream\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]
