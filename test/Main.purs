module Test.Main where

import Prelude
import Test.Assert (ASSERT)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Decosim.Sim.HaldaneanAlgorithmSpec as HaldaneanAlgorithmSpec
import Decosim.Sim.HaldaneadModelSpec as HaldaneadModelSpec
import Decosim.Sim.DiveProfileSpec as DiveProfileSpec
import Decosim.Sim.DiveTables (printDiveTables)

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  HaldaneanAlgorithmSpec.spec
  HaldaneadModelSpec.spec
  DiveProfileSpec.airSpec
  DiveProfileSpec.trimixIISpec
  printDiveTables
