module Decosim.Sim.HaldaneanAlgorithm where

import Prelude
import Data.Array as A
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tagged (Tagged, tag)
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds), convertDuration)
import Math as Math

import Decosim.Sim.Types(Bar(Bar), Meters(Meters), He, N2, toNumber)


-- | Constants for a Tissue.
-- |
-- | - `halflife`: Absorption rate of gas into the tissue
-- |               Some models use different values for on-gassing and off-gassing (for
-- |               conservative model, off-gassing is slower than on-gasing)
-- |               In this model, all other values can be derived from the halflife.
-- | - `mo`: Critical tissue pressure - max tolerated at the surface.
-- |         Scale by depth (in bar) to get the max tolerated at any depth.
-- | - `k`: Halftime constant
-- |        `(-Math.log(0.5) / halflife)`
type TissueConf t =
  { halflife ∷ Tagged t Minutes
  , a        ∷ Tagged t Number -- Bar?
  , b        ∷ Tagged t Number
  , mo       ∷ Tagged t Bar
  , k        ∷ Tagged t Number
  }


maxtime ∷ Minutes
maxtime = Minutes 99.0


-- | partial pressure of water vapour in lung
pw ∷ Bar
pw = Bar 0.0567

-- | pressure at surface (assuming diving at sea-level)
surfaceAta ∷ Bar
surfaceAta = Bar 1.0

barToDepth ∷ Bar → Meters
barToDepth bar =
  Meters $ 10.0 * (toNumber bar - toNumber surfaceAta)

depthToBar ∷ Meters → Bar
depthToBar depth =
  Bar $ (toNumber depth / 10.0 + toNumber surfaceAta)

toTissueConf ∷ ∀ t. Tagged t Bar → Minutes → TissueConf t
--toTissueConf ∷ _
toTissueConf surfacePa halflife =
    { halflife : tag halflife
    , a        : tag a
    , b        : tag b
    , mo       : tag $ Bar mo
    , k        : tag k
    }
  where
    tht = toNumber halflife
    a   = 2.0   * (tht `Math.pow` (-1.0 / 3.0))
    b   = 1.005 - (tht `Math.pow` (-1.0 / 2.0))
    mo  = (toNumber surfacePa / b + a)
    k   = -Math.log(0.5) / tht

-- size between stops
stopDelta ∷ Meters
stopDelta = Meters 3.0

-- |  Calculated ambient pressure for a gas with given percentage and at given depth
calculatePa ∷ ∀ t. Tagged t Number → Meters → Tagged t Bar
calculatePa partialPercentage depth =
  tag $ Bar $ toNumber partialPercentage * (toNumber (depthToBar depth) - toNumber pw)


-- | The reverse of calculatePa
calculateDepthForGivenPa ∷ ∀ t. Tagged t Number → Tagged t Bar → Meters
calculateDepthForGivenPa partialPercentage pa =
  barToDepth $ Bar (toNumber pa / toNumber partialPercentage) <> pw

-- | This is the haldanean equation.
-- | Calulates gas pressure in tissue
-- | Note it's applicable to nitrogen and helium - not oxygen since it is metabolised
-- | `Pt  = Pt-1+ (Pa - Pt-1)(1- exp(t ln(0.5)/T1/2 )`
calculatePt ∷ ∀ t. Tagged t Bar → Tagged t Bar → Minutes → TissueConf t → Tagged t Bar
calculatePt previousPt' pa' timeStep' tissueConf =
    tag $ Bar $ previousPt + (pa - previousPt) * (1.0 - Math.exp(-k * timeStep))
  where
    previousPt = toNumber previousPt'
    pa         = toNumber pa'
    timeStep   = toNumber timeStep'
    k          = toNumber tissueConf.k

-- | This is the schreiner equation.
-- | We get the 'Instantaneous Equation' calculatePt, when `r=0`
calculatePt2 ∷ ∀ t. Tagged t Bar → Tagged t Bar → Number → Tagged t Number → Minutes → TissueConf t → Tagged t Bar
calculatePt2 initPt' initPa' descentRate partialPercentage timeStep' tissueConf =
    tag $ Bar $ initPa + r * (1.0 - 1.0 / k) - (initPa - initPt - r / k) * Math.exp(- k * timeStep)
  where
    r        = descentRate * toNumber partialPercentage
    initPa   = toNumber initPa'
    initPt   = toNumber initPt'
    timeStep = toNumber timeStep'
    k        = toNumber tissueConf.k



-- | Calculates the time to reach the critical level (where we can't go safely to the surface without getting 'bubble fiz')
-- | If we are already over the critical level, this is the time for the nitrogen level to drop to the critical level
-- |   `td  = T1/2 ln((Mo - Pa )/( Pt-1 - Pa ))/ln(0.5)`
-- |   if Mo>Pa, we can't get saturated here (Pa is not enough to raise Pt-1 to Mo)
-- |   if Mo<Pa, if Pt-1 <Pa, it is raising, so time is noDecoTime, time to saturation (and when decompression is required)
-- |   if Mo<Pa, if Pt-1 > Pa, we are already saturated but it is dropping, time to saturation lime is time required to decompress (and be safe again)
calculateTimeToSaturation ∷ ∀ t. Tagged t Minutes → Tagged t Bar → Tagged t Bar → Tagged t Bar → Maybe Minutes
calculateTimeToSaturation tissueHalfLife mo pt pa =
  if    pt /= pa   -- else at equilibrium
     && aux > zero -- i.e. mo > pt > pa or mo < pt < pa. else nPa < mo, never can reach critical level
     && timeToSaturation >= mempty -- i.e. we're already saturated
    then Just timeToSaturation
    else
      Nothing
  where
    aux = (toNumber mo - toNumber pa) / (toNumber pt - toNumber pa)
    timeToSaturation = Minutes $ Math.log(aux) * toNumber tissueHalfLife / Math.log(0.5)

noDecoTime ∷ TissueConf N2 → TissueConf He → Tagged N2 Bar → Tagged N2 Bar → Tagged He Bar → Tagged He Bar → Minutes
noDecoTime n2Conf heConf n2Pt n2Pa hePt hePa =
    foldl min maxtime $ A.catMaybes [n2NoDecoTime, heNoDecoTime]
  where
    n2NoDecoTime = calculateTimeToSaturation n2Conf.halflife n2Conf.mo n2Pt n2Pa
    heNoDecoTime = calculateTimeToSaturation heConf.halflife heConf.mo hePt hePa


decoStopTime ∷ Meters → Tagged N2 Number → Tagged He Number → TissueConf N2 → TissueConf He → Tagged N2 Bar → Tagged He Bar → Minutes
decoStopTime stopDepth n2Per hePer n2Conf heConf n2Pt hePt =
    foldl max mempty $ A.catMaybes [map toNextMin n2DecoTime, map toNextMin heDecoTime]
  where
    n2PaDecoDepth = calculatePa n2Per stopDepth
    hePaDecoDepth = calculatePa hePer stopDepth
    nextStop      = max mempty (Meters $ toNumber stopDepth - toNumber stopDelta)
    toleratedN2Pa = tag $ Bar $ toNumber n2Conf.mo * toNumber (depthToBar nextStop)
    toleratedHePa = tag $ Bar $ toNumber heConf.mo * toNumber (depthToBar nextStop)
    n2DecoTime    = calculateTimeToSaturation n2Conf.halflife toleratedN2Pa n2Pt n2PaDecoDepth
    heDecoTime    = calculateTimeToSaturation heConf.halflife toleratedHePa hePt hePaDecoDepth
    -- if we don't round up, we may get an infinite loop where rounding sends nextDepth to be the same
    --toNextMin t =  t -- Minutes $ Math.ceil $ unwrap t
    -- or to nearest 10 seconds
    toNextMin t =  convertDuration $ Seconds $ 10.0 * (Math.ceil ((unwrap $ convertDuration t ∷ Seconds) / 10.0))

-- | this is the safest depth we can go up to without our tissue pressure exceeding the ambient pressure by the critical amount to cause 'bubble fiz'
-- | i.e. to what depth will Pt/Pa > tissueCriticalGradient
-- | since Pa = nitrogenPercentage(depth/10 +1)
-- | and tissueCriticalGradient = mo/nPercentage
-- | we get depth = 10*((Pt-1/mo)-1)
-- |
-- | this is true for nitrogen and helium
calculateSafeAscentDepth ∷ ∀ t. Tagged t Bar → TissueConf t → Meters
calculateSafeAscentDepth pt' tissueConf =
    barToDepth $ Bar (pt / mo)
  where
    pt = toNumber pt'
    mo = toNumber tissueConf.mo


-- | More conservative than `ascentCeiling'`.
-- | Takes the max of n2 and he.
-- | (More conservative again would be to use n2Conf for both n2 and he)
ascentCeiling ∷ Tagged N2 Bar → Tagged He Bar → TissueConf N2 → TissueConf He → Meters
ascentCeiling n2Pt hePt n2Conf heConf =
  max (calculateSafeAscentDepth n2Pt n2Conf)
      (calculateSafeAscentDepth hePt heConf)

-- | Less conservative than `ascentCeiling`.
-- | Scales between n2 and he.
ascentCeiling' ∷ Tagged N2 Bar → Tagged He Bar → TissueConf N2 → TissueConf He → Meters
ascentCeiling' n2Pt' hePt' n2Conf heConf =
    barToDepth (Bar ceilingAta)
  where
    n2Pt = toNumber n2Pt'
    hePt = toNumber hePt'
    n2A  = toNumber n2Conf.a
    n2B  = toNumber n2Conf.b
    heA  = toNumber heConf.a
    heB  = toNumber heConf.b
    a = (n2A * n2Pt + heA * hePt) / (n2Pt + hePt)
    b = (n2B * n2Pt + heB * hePt) / (n2Pt + hePt)
    ceilingAta = ((n2Pt + hePt) - a) * b
