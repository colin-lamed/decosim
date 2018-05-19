module Decosim.Sim.DiveTables
  ( printDiveTables
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((..))
import Data.Int as I
import Data.Foldable (for_)

import Decosim.Sim.Types (Meters(Meters))
import Decosim.Sim.HaldaneanModel


printDiveTables :: forall e. Eff (console :: CONSOLE | e) Unit
printDiveTables = do
    diveTable Air
    diveTable NitroxI
    diveTable NitroxII
    diveTable Oxygen
    diveTable TrimixI
    diveTable TrimixII
  where
    diveTable gasMix = do
      log $ "\nDiveTable " <> show gasMix
      let model = initModel { gasMix = gasMix }
      for_ (0..20) \i -> do
        let depth = Meters $ 5.0 * I.toNumber i
        if depth <= maxOperatingDepth gasMix
          then do let timeMins = noDecoTimeAtDepth model depth
                  log $ show depth <> " - " <> show timeMins
          else pure unit
