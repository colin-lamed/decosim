module Decosim.Sim where

import Prelude
import Data.Array as A
import Data.Newtype (over)
import Data.Time.Duration (Minutes(..))

import Decosim.Sim.Types (Meters(..), toNumber)
import Decosim.Sim.HaldaneanModel (GasMix, Model, initModel, updateModel)


type Sim =
  { time             ∷ Minutes
  , direction        ∷ Direction
  , paused           ∷ Boolean
  , timeFactor       ∷ Number
  , maxViewableDepth ∷ Meters
  , maxViewableTime  ∷ Minutes
  , model            ∷ Model
  , historicData     ∷ Array HistoricData
  }

data Direction
  = Up
  | Down
  | Horizontal

derive instance eqDirection ∷ Eq Direction

-- (speed could depend on depth, e.g. doubles under 20m)
depthChange ∷ Direction → Number   -- meters/minute
depthChange Up         = -15.0
depthChange Down       =  30.0
depthChange Horizontal =  0.0

type HistoricData =
  { depth        ∷ Meters
  , time         ∷ Minutes
  , gasMix       ∷ GasMix
  }

defaultTimeFactor ∷ Number
defaultTimeFactor = 1.0

initSim ∷ Sim
initSim =
  { time             : mempty
  , direction        : Down
  , paused           : true
  , timeFactor       : defaultTimeFactor
  , maxViewableDepth : Meters 70.0
  , maxViewableTime  : Minutes 30.0
  , model            : initModel
  , historicData     : []
  }

update ∷ Minutes → Sim → Sim
update dt sim =
    sim { time             = time
        , maxViewableDepth = if depth > over Meters  (_ * 0.8) sim.maxViewableDepth then sim.maxViewableDepth <> Meters  15.0 else sim.maxViewableDepth
        , maxViewableTime  = if time  > over Minutes (_ * 0.8) sim.maxViewableTime  then sim.maxViewableTime  <> Minutes 30.0 else sim.maxViewableTime
        , model            = updateModel depth timeStep sim.model
        , historicData     = sim.historicData `A.snoc` { depth, time, gasMix }
        }
  where
    timeStep = Minutes $ toNumber dt * sim.timeFactor
    time     = sim.time <> timeStep
    depth    = max mempty (Meters $ toNumber sim.model.depth + depthChange sim.direction * toNumber timeStep)
    gasMix   = sim.model.gasMix
