module Decosim.Sim.HaldaneanModel where

import Prelude

import Data.Array ((..))
import Data.Array as A
import Data.Array.Partial (head)
import Data.Either (Either(Left,Right))
import Data.Foldable (foldl)
import Data.Int as I
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tagged (Tagged, tag)
import Data.Time.Duration (Minutes(Minutes))
import Math as Math
import Partial.Unsafe (unsafePartial)

import Decosim.Sim.HaldaneanAlgorithm as HA
import Decosim.Sim.Types (Bar(Bar), Meters(Meters), He, N2, O2, toNumber)


-- | paO2 below which leads to hypoxia
o2HypoxiaAta ∷ Tagged O2 Bar
o2HypoxiaAta = tag $ Bar 0.1

-- | and 1.4 for cold water
o2ToxicityAta ∷ Tagged O2 Bar
o2ToxicityAta = tag $ Bar 1.6

numTissues ∷ Int
numTissues = 16


-- https://en.wikipedia.org/wiki/B%C3%BChlmann_decompression_algorithm
-- n2Halflifes = map (Minutes) [4.0, 8.0, 12.5, 18.5, 27.0, 38.3, 54.3, 77.0, 109.0, 146.0, 187.0, 239.0, 305.0, 390.0, 498.0, 635.0]
-- an2 = [1.2599, 1.0000, 0.8618, 0.7562, 0.6667, 0.5933, 0.5282, 0.4701, 0.4187, 0.3798, 0.3497, 0.3223, 0.2971, 0.2737, 0.2523, 0.2327]
-- bn2 = [0.5050, 0.6514, 0.7222, 0.7725, 0.8125, 0.8434, 0.8693, 0.8910, 0.9092, 0.9222, 0.9319, 0.9403, 0.9477, 0.9544, 0.9602, 0.9653]
-- n2Mos = A.zipWith (HA.calculateCriticalPressure surfaceN2Pa) an2 bn2
--
-- heHalflifes = map (Minutes) [1.5, 3.0, 4.7, 7.0, 10.2, 14.5, 20.5, 29.1, 41.1, 55.1, 70.6, 90.2, 115.1, 147.2, 187.9, 239.6]
-- ahe = [1.7435, 1.3838, 1.1925, 1.0465, 0.9226, 0.8211, 0.7309, 0.6506, 0.5794, 0.5256, 0.4840, 0.4460, 0.4112, 0.3788, 0.3492, 0.3220]
-- bhe = [0.1911, 0.4295, 0.5446, 0.6265, 0.6917, 0.7420, 0.7841, 0.8195, 0.8491, 0.8703, 0.8860, 0.8997, 0.9118, 0.9226, 0.9321, 0.9404]
-- heMos = A.zipWith (HA.calculateCriticalPressure surfaceHePa) ahe bhe


n2Confs ∷ Array (HA.TissueConf N2)
n2Confs = map (HA.toTissueConf surfaceN2Pa <<< Minutes) [4.0, 8.0, 12.5, 18.5, 27.0, 38.3, 54.3, 77.0, 109.0, 146.0, 187.0, 239.0, 305.0, 390.0, 498.0, 635.0]

heConfs ∷ Array (HA.TissueConf He)
heConfs = map (HA.toTissueConf surfaceHePa <<< Minutes) [1.5, 3.0, 4.7, 7.0, 10.2, 14.5, 20.5, 29.1, 41.1, 55.1, 70.6, 90.2, 115.1, 147.2, 187.9, 239.6]


n2Conf ∷ TissueCompartment → HA.TissueConf N2
n2Conf tissue =
  n2Confs `at` tissue.i

heConf ∷ TissueCompartment → HA.TissueConf He
heConf tissue =
  heConfs `at` tissue.i

at ∷ ∀ a. Array a → Int → a
at array i = unsafePartial $ A.unsafeIndex array i



surfaceN2Pa ∷ Tagged N2 Bar
surfaceN2Pa = HA.calculatePa (tag 0.79) mempty

surfaceHePa ∷ Tagged He Bar
surfaceHePa = HA.calculatePa (tag 0.0) mempty

data GasMix
  = Air
  | NitroxI
  | NitroxII
  | Oxygen
  | TrimixI
  | TrimixII


derive instance eqGasMix ∷ Eq GasMix
instance showGasMix ∷ Show GasMix where
  show Air      = "Air"
  show NitroxI  = "Nitrox I (32%)"
  show NitroxII = "Nitrox II (36%)"
  show Oxygen   = "100% Oxygen"
  show TrimixI  = "Trimix I (O20%, N45%)"
  show TrimixII = "Trimix II (O12%, N28%)"

allGasMixes ∷ Array GasMix
allGasMixes = [ Air, NitroxI, NitroxII, Oxygen, TrimixI, TrimixII ]

percentO2 ∷ GasMix → Tagged O2 Number
percentO2 Air      = tag 0.21
percentO2 NitroxI  = tag 0.32
percentO2 NitroxII = tag 0.36
percentO2 Oxygen   = tag 1.0
percentO2 TrimixI  = tag 0.2
percentO2 TrimixII = tag 0.12

percentN2 ∷ GasMix → Tagged N2 Number
percentN2 Air       = tag 0.79
percentN2 NitroxI   = tag 0.68
percentN2 NitroxII  = tag 0.64
percentN2 Oxygen    = tag 0.0
percentN2 TrimixI   = tag 0.45
percentN2 TrimixII  = tag 0.28

percentHe ∷ GasMix → Tagged He Number
percentHe g =
  tag $ 1.0 - (toNumber $ percentO2 g) - (toNumber $ percentN2 g)

getGasMixColour ∷ GasMix → String
getGasMixColour Air      = "rgb(56,202,239)"
getGasMixColour NitroxI  = "rgb(87,239,190)"
getGasMixColour NitroxII = "rgb(56,156,124)"
getGasMixColour Oxygen   = "rgb(195,223,221)"
getGasMixColour TrimixI  = "rgb(223,177,221)"
getGasMixColour TrimixII = "rgb(151,120,149)"


type Model =
  { gasMix  ∷ GasMix
  , depth   ∷ Meters
  , tissues ∷ Array TissueCompartment
  }

showModel ∷ Model → String
showModel model =
    "{ gasMix: " <> show model.gasMix <> ", depth: " <> show model.depth <> ", tissues: {" <> L.intercalate "," (map show model.tissues) <> "} }"


data TissueDeco
  = TDNoDecoTime Minutes
  | TDDecoStop Meters

instance showTissueDeco ∷ Show TissueDeco where
  show (TDNoDecoTime s) = "(TDNoDecoTime " <> show s <> ")"
  show (TDDecoStop   m) = "(TDDecoStop "   <> show m <> ")"


type TissueCompartment =
  { i    ∷ Int
  , n2Pt ∷ Tagged N2 Bar
  , hePt ∷ Tagged He Bar
  , deco ∷ TissueDeco
  }

initModel ∷ Model
initModel =
    { gasMix  : gasMix
    , depth   : Meters 0.0
    , tissues : map initTissue (0..(numTissues - 1))
    }

  where
    gasMix = Air
    -- initally all tissues are at surface level air nitrogen pressure levels
    initTissue ∷ Int → TissueCompartment
    initTissue i =
      { i    : i
      , n2Pt : surfaceN2Pa
      , hePt : surfaceHePa
      , deco : TDNoDecoTime mempty
      }

-- | Calculates the gas pressure in tissue
--   and other calculations based on this
--   the get methods can then be used to read all the results
updateModel ∷ Meters → Minutes → Model → Model
updateModel depth timeStep model =
    model
      { depth    = depth
      , tissues = map (updateTissue timeStep model n2Pa hePa) model.tissues
      }
  where
    n2Pa = HA.calculatePa (percentN2 model.gasMix) depth
    hePa = HA.calculatePa (percentHe model.gasMix) depth


updateTissue ∷ Minutes → Model → Tagged N2 Bar → Tagged He Bar → TissueCompartment → TissueCompartment
updateTissue timeStep model n2Pa hePa tissue =
    tissue
      { n2Pt = n2Pt
      , hePt = hePt
      , deco = if ascentCeiling > mempty
                then TDDecoStop ascentCeiling
                else let noDecoTime = HA.noDecoTime (n2Conf tissue) (heConf tissue) n2Pt n2Pa hePt hePa
                     in TDNoDecoTime noDecoTime
      }
  where
    n2Pt = HA.calculatePt tissue.n2Pt n2Pa timeStep (n2Conf tissue)
    hePt = HA.calculatePt tissue.hePt hePa timeStep (heConf tissue)
    ascentCeiling = HA.ascentCeiling n2Pt hePt (n2Conf tissue) (heConf tissue)
    -- ascentCeiling = HA.ascentCeiling' n2Pt hePt (n2Conf tissue) (heConf tissue)


decoRequired ∷ Model → Boolean
decoRequired model =
  A.any (\t → case t.deco of
                 TDNoDecoTime _ → false
                 TDDecoStop _   → true
         ) model.tissues

noDecoTime ∷ Model → Minutes
noDecoTime model =
  min' $ map (\t → case t.deco of
                      TDNoDecoTime noDecoTime → noDecoTime
                      TDDecoStop _            → mempty
             ) model.tissues

safeAscentDepth ∷ Model → Meters
safeAscentDepth model =
  max' $ map (\t → case t.deco of
                      TDNoDecoTime _ → mempty
                      TDDecoStop sad → sad
             ) model.tissues

-- | deco stop depth as the next depth divisible by 3
calculateDecoStopDepth ∷ Meters → Int
calculateDecoStopDepth sad =
    multiplesOf3 * 3
  where
    multiplesOf3' = I.floor (toNumber sad) / 3
    multiplesOf3 = multiplesOf3' + (if (toNumber sad Math.% 3.0) == 0.0 then 0 else 1)


type DecoStop =
  { depth  ∷ Meters
  , time   ∷ Minutes
  , gasMix ∷ GasMix
  }

decoStops ∷ Model → Either String (Array DecoStop)
decoStops model =
    go model []
  where
    go model acc =
      let stop = { depth  : decoStopDepth model
                 , time   : decoStopTime  model
                 , gasMix : model.gasMix
                 }
      in if stop.depth <= mempty
            then Right acc
          else let model1 = (updateModel stop.depth stop.time model) { gasMix = stop.gasMix }
                   nextStop = { depth  : decoStopDepth model1
                              , time   : decoStopTime  model1
                              , gasMix : model1.gasMix
                              }
               in if nextStop.depth >= stop.depth
                   -- only falling to Air if current gas mix will not work
                   -- TODO consider all gas mixes - pick which one reduces the time the most (as long as within operating depth)?
                   then if stop.gasMix == Air
                        then Left ("No stop available! Waiting at " <> show stop <> " will lead to a deeper stop " <> show nextStop)
                        else let nextGasMix = Air
                             in if minOperatingDepth nextGasMix < stop.depth && maxOperatingDepth nextGasMix > stop.depth
                               then go (model { gasMix = nextGasMix }) acc
                               else Left ("No stop available! Waiting at " <> show stop <> " will lead to a deeper stop " <> show nextStop <> " and can't switch to " <> show nextGasMix <> " - outside operating depth")
                  else go model1 (acc `A.snoc` stop)
    decoStopDepth ∷ Model → Meters
    decoStopDepth model =
      Meters $ I.toNumber $ calculateDecoStopDepth $ safeAscentDepth model

    decoStopTime ∷ Model → Minutes
    decoStopTime model =
      let depth = decoStopDepth model
      in if depth <= mempty then mempty
         else let model1              = updateModel depth mempty model
                  decoStopTime tissue = HA.decoStopTime depth (percentN2 model.gasMix) (percentHe model.gasMix) (n2Conf tissue) (heConf tissue) tissue.n2Pt tissue.hePt
              in max' $ map decoStopTime model1.tissues

firstDecoStop ∷ Model → Maybe DecoStop
firstDecoStop model =
  case decoStops model of
    Left msg        → Nothing
    Right decoStops → A.head decoStops


n2Pa ∷ Model → Tagged N2 Bar
n2Pa model =
  HA.calculatePa (percentN2 model.gasMix) model.depth

hePa ∷ Model → Tagged He Bar
hePa model =
  HA.calculatePa (percentHe model.gasMix) model.depth

o2Pa ∷ Model → Tagged O2 Bar
o2Pa model =
  HA.calculatePa (percentO2 model.gasMix) model.depth

noDecoTimeAtDepth ∷ Model → Meters → Minutes
noDecoTimeAtDepth model depth =
  noDecoTime (updateModel depth mempty model)

-- depth where O2 leads to hypoxia
minOperatingDepth ∷ GasMix → Meters
minOperatingDepth gasMix =
  HA.calculateDepthForGivenPa (percentO2 gasMix) o2HypoxiaAta

-- | Depth where O2 becomes toxic.
-- This should actually be a function of time. : 1.6bar - 45mins, 1.5 bar - 120mins, 1.4 bar - 150mins, 1.3 bar -180mins, 1.2 bar - 210mins
-- 2-16 hours of oxygen at 1.0 ATA, 8-14 hours at 1.5 ATA, and 3-6 hours at 2.0 ATA before developing mild symptoms
maxOperatingDepth ∷ GasMix → Meters
maxOperatingDepth gasMix =
  HA.calculateDepthForGivenPa (percentO2 gasMix) o2ToxicityAta

-- can't use fold with since Data.Ord.Min/Max since Minutes/Meters are not Bounded
-- could use NonEmptyArray (but noisier code)
min' ∷ ∀ a. Ord a ⇒ Array a → a
min' a = foldl min (unsafePartial head a) a

max' ∷ ∀ a. Ord a ⇒ Array a → a
max' a = foldl max (unsafePartial head a) a
