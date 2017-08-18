module Test.Main where

import Jack.Runner (jackMain)
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow, log)
import Control.Monad.Eff.Random (RANDOM)

import Test.Spans
import Jack

main :: forall e. Eff (console :: CONSOLE, random :: RANDOM | e) Unit
main =
  do log "begin"
     jackMain ["Test.Spans"]
     log "end"
  {-
  do log "apa"
     st <- sampleTree 5 1 genSpans
     logShow (map (showSpans <<< outcome) st)
     log "bepa"
     -}
