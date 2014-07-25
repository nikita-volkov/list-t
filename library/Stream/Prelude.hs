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

-- custom
-------------------------
import qualified Debug.Trace.LocationTH

bug = [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"stream\" package bug: " :: String

bottom = [e| $bug "Bottom evaluated" |]
