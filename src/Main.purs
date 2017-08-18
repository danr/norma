module Main where

import Prelude
import Spans
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (const "Hello" (autoRevert [] (modify 1 2 "apa" (init (tokenize "hello there")))))
