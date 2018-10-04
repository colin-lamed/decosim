module Test.Main where

import Prelude
import Effect (Effect)
import Decosim.Sim.HaldaneanAlgorithmSpec as HaldaneanAlgorithmSpec
import Decosim.Sim.HaldaneadModelSpec as HaldaneadModelSpec
import Decosim.Sim.DiveProfileSpec as DiveProfileSpec
import Decosim.Sim.DiveTables (printDiveTables)

main ∷ Effect Unit
main = do
  HaldaneanAlgorithmSpec.spec
  HaldaneadModelSpec.spec
  DiveProfileSpec.airSpec
  DiveProfileSpec.trimixIISpec
  printDiveTables
