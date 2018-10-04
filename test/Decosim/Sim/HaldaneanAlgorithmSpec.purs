module Decosim.Sim.HaldaneanAlgorithmSpec where

import Prelude
import Data.Array ((..))
import Data.Foldable (traverse_, foldM)
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tagged (tag, untag)
import Data.Time.Duration (Minutes(Minutes))
import Effect (Effect)
import Effect.Console (log)
import Math as Math
import Test.Assert (assert')

import Decosim.Sim.Types (Bar(Bar), Meters(Meters), toNumber)
import Decosim.Sim.HaldaneanAlgorithm as HA


spec ∷ Effect Unit
spec = do

  log "\nHaldaneanAlgorithmSpec:"

  let partialPercentage = 0.79

  -- tolerance for water vapour
  let approxEq ∷ Bar → Bar → Boolean
      approxEq b1 b2 =
        toNumber b1 - toNumber b2 < 0.1

  -- testCalculatePa
  -- every 10m of depth, partialPercentage increases by the initial amount
  let testCalculatePa i = assert' "testCalculatePa failed" $
                            (Bar $ (I.toNumber i + 1.0) * partialPercentage)
                            `approxEq`
                            unwrap (HA.calculatePa (tag partialPercentage) (Meters $ 10.0 * I.toNumber i))
  traverse_ testCalculatePa (0..100)

  -- testCalculateDepthForGivenPa
  log "testCalculateDepthForGivenPa"
  -- calculateDepthForGivenPa is the inverse of calculatePa
  let testCalculateDepthForGivenPa i =
            let depth  = 10 * i
                depth2 = I.round $ toNumber $ HA.calculateDepthForGivenPa (tag partialPercentage) (HA.calculatePa (tag partialPercentage) (Meters $ I.toNumber depth))
            in assert' ("testCalculateDepthForGivenPa failed for i "<> show i <> " " <> show depth <> " /= " <> show depth2) $ depth == depth2
  traverse_ testCalculateDepthForGivenPa (0..100)


  -- testCalculatePtDoubleDoubleIntDouble
  log "testCalculatePtDoubleDoubleIntDouble"
  let pa         = Bar 1.0
      timeStep   = Minutes 1.0
      tissueConf = HA.toTissueConf (HA.calculatePa (tag partialPercentage) mempty) (Minutes 1.0)
  let testCalculatePtDoubleDoubleIntDouble previousPt i = do
      --check it is a half life
        let pt = HA.calculatePt (tag previousPt) (tag pa) timeStep tissueConf
        assert' "testCalculatePtDoubleDoubleIntDouble failed" $ untag pt `approxEq` max previousPt (Bar $ toNumber pa - (Math.abs (toNumber previousPt - toNumber pa) / 2.0))
        pure (untag pt)
  void $ foldM testCalculatePtDoubleDoubleIntDouble (Bar 2.0) (0..100)


  -- testCalculateTimeToSaturation
  log "testCalculateTimeToSaturation"
  let testCalculateTimeToSaturation record expectedTime =
        let time = HA.calculateTimeToSaturation record.tissueHalflife record.mo record.pt record.pa
        in assert' ("timeToSaturation for " <> show record <> " should be " <> show expectedTime <> " but was " <> show time) $ expectedTime == time

  -- if Mo>Pa, we can't get saturated here (Pa is not enough to raise Pt to Mo)
  -- if Mo<Pa, if Pt <Pa, it is raising, so time is noDecoTime, time to saturation (and when decompression is required)
  -- if Mo<Pa, if Pt > Pa, we are already saturated but it is dropping, time to saturation time is time required to decompress (and be safe again)

  --  if Mo>Pa, and pt < mo, we can't get saturated here (Pa is not enough to raise Pt-1 to Mo)
  testCalculateTimeToSaturation { mo: tag $ Bar 2.0, pa: tag $ Bar 1.0, pt: tag $ Bar 0.5, tissueHalflife: tag $ Minutes 1.0} Nothing
  testCalculateTimeToSaturation { mo: tag $ Bar 2.0, pa: tag $ Bar 1.0, pt: tag $ Bar 1.5, tissueHalflife: tag $ Minutes 1.0} Nothing
  -- but if pt is greater than mo, we are calculating time till pt drops to mo
  -- since mo is right in the middle of pa and pt, should take halflife time to reach mo
  testCalculateTimeToSaturation { mo: tag $ Bar 2.0, pa: tag $ Bar 1.0, pt: tag $ Bar 3.0, tissueHalflife: tag $ Minutes 1.0} (Just (Minutes 1.0))
  -- mo<pa:
  -- if pt <mo, it is rising, so time is noDecoTime, time to saturation (and when decompression is required)
  -- since mo is right in the middle of pa and pt, should take halflife time to reach mo
  testCalculateTimeToSaturation { mo: tag $ Bar 2.0, pa: tag $ Bar 3.0, pt: tag $ Bar 1.0, tissueHalflife: tag $ Minutes 1.0} (Just (Minutes 1.0))
  -- since mo is right in the middle of pa and pt, should take halflife time to reach mo
  testCalculateTimeToSaturation { mo: tag $ Bar 2.0, pa: tag $ Bar 1.0, pt: tag $ Bar 3.0, tissueHalflife: tag $ Minutes 1.0} (Just (Minutes 1.0))

  -- if mo < pa,  and pt > mo, we are not heading towards saturation
  testCalculateTimeToSaturation { mo: tag $ Bar 1.0, pa: tag $ Bar 3.0, pt: tag $ Bar 2.0, tissueHalflife: tag $ Minutes 1.0} Nothing
  testCalculateTimeToSaturation { mo: tag $ Bar 1.0, pa: tag $ Bar 2.0, pt: tag $ Bar 3.0, tissueHalflife: tag $ Minutes 1.0} Nothing


  -- testCalculateSafeAscentDepth
  log "testCalculateSafeAscentDepth"

  let -- mo and depth don't matter
      surfaceN2Pa = HA.calculatePa (tag partialPercentage) mempty
      tissueConf = HA.toTissueConf surfaceN2Pa (Minutes 27.0)-- mo = Bar 1.6
      depth = Meters 40.0
      -- if we are below the pa at a certain depth, our sad should be shallower than that depth
      pt1 = tag $ Bar $ (toNumber tissueConf.mo) * (toNumber depth / 10.0 + 1.0) - 1.0
  assert' "calculateSafeAscentDepth failed" $ (HA.calculateSafeAscentDepth pt1 tissueConf) < depth

      -- if we are above the pa at a certain depth, our sad should be deeper than that depth
  let pt2 = tag $ Bar $ (toNumber tissueConf.mo) * (toNumber depth / 10.0 + 1.0) + 1.0
  assert' "calculateSafeAscentDepth failed" $ (HA.calculateSafeAscentDepth pt2 tissueConf) > depth
