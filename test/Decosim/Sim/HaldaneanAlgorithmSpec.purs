module Decosim.Sim.HaldaneanAlgorithmSpec where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((..))
import Data.Foldable (traverse_, foldM)
import Data.Functor.Tagged (tagged, untagged)
import Data.Int as I
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Record.ShowRecord (showRecord)
import Data.Time.Duration (Minutes(Minutes))
import Math as Math
import Test.Assert (ASSERT, assert')

import Decosim.Sim.Types (Bar(Bar), Meters(Meters), extract)
import Decosim.Sim.HaldaneanAlgorithm as HA


spec ∷ ∀ e. Eff (console ∷ CONSOLE, assert :: ASSERT | e) Unit
spec = do

  log "\nHaldaneanAlgorithmSpec:"

  let partialPercentage = 0.79

  -- tolerance for water vapour
  let approxEq :: Bar -> Bar -> Boolean
      approxEq b1 b2 =
        b1 - b2 < Bar 0.1

  -- testCalculatePa
  -- every 10m of depth, partialPercentage increases by the initial amount
  let testCalculatePa i = assert' "testCalculatePa failed" $
                            Bar ((I.toNumber i + 1.0) * partialPercentage)
                            `approxEq`
                            untagged (HA.calculatePa (tagged partialPercentage) (Meters $ 10.0 * I.toNumber i))
  traverse_ testCalculatePa (0..100)

  -- testCalculateDepthForGivenPa
  log "testCalculateDepthForGivenPa"
  -- calculateDepthForGivenPa is the inverse of calculatePa
  let testCalculateDepthForGivenPa i =
            let depth  = 10 * i
                depth2 = I.round $ unwrap $ HA.calculateDepthForGivenPa (tagged partialPercentage) (HA.calculatePa (tagged partialPercentage) (Meters $ I.toNumber depth))
            in assert' ("testCalculateDepthForGivenPa failed for i "<> show i <> " " <> show depth <> " /= " <> show depth2) $ depth == depth2
  traverse_ testCalculateDepthForGivenPa (0..100)


  -- testCalculatePtDoubleDoubleIntDouble
  log "testCalculatePtDoubleDoubleIntDouble"
  let pa         = Bar 1.0
      timeStep   = Minutes 1.0
      tissueConf = HA.toTissueConf (HA.calculatePa (tagged partialPercentage) zero) (Minutes 1.0)
  let testCalculatePtDoubleDoubleIntDouble previousPt i = do
      --check it is a half life
        let pt = HA.calculatePt (tagged previousPt) (tagged pa) timeStep tissueConf
        assert' "testCalculatePtDoubleDoubleIntDouble failed" $ untagged pt `approxEq` max previousPt (pa - over Bar (\b -> Math.abs b / 2.0) (previousPt - pa))
        pure (untagged pt)
  void $ foldM testCalculatePtDoubleDoubleIntDouble (Bar 2.0) (0..100)


  -- testCalculateTimeToSaturation
  log "testCalculateTimeToSaturation"
  let testCalculateTimeToSaturation record expectedTime =
        let time = HA.calculateTimeToSaturation record.tissueHalflife record.mo record.pt record.pa
        in assert' ("timeToSaturation for " <> showRecord record <> " should be " <> show expectedTime <> " but was " <> show time) $ expectedTime == time

  -- if Mo>Pa, we can't get saturated here (Pa is not enough to raise Pt to Mo)
  -- if Mo<Pa, if Pt <Pa, it is raising, so time is noDecoTime, time to saturation (and when decompression is required)
  -- if Mo<Pa, if Pt > Pa, we are already saturated but it is dropping, time to saturation time is time required to decompress (and be safe again)

  --  if Mo>Pa, and pt < mo, we can't get saturated here (Pa is not enough to raise Pt-1 to Mo)
  testCalculateTimeToSaturation { mo: tagged $ Bar 2.0, pa: tagged $ Bar 1.0, pt: tagged $ Bar 0.5, tissueHalflife: tagged $ Minutes 1.0} Nothing
  testCalculateTimeToSaturation { mo: tagged $ Bar 2.0, pa: tagged $ Bar 1.0, pt: tagged $ Bar 1.5, tissueHalflife: tagged $ Minutes 1.0} Nothing
  -- but if pt is greater than mo, we are calculating time till pt drops to mo
  -- since mo is right in the middle of pa and pt, should take halflife time to reach mo
  testCalculateTimeToSaturation { mo: tagged $ Bar 2.0, pa: tagged $ Bar 1.0, pt: tagged $ Bar 3.0, tissueHalflife: tagged $ Minutes 1.0} (Just (Minutes 1.0))
  -- mo<pa:
  -- if pt <mo, it is rising, so time is noDecoTime, time to saturation (and when decompression is required)
  -- since mo is right in the middle of pa and pt, should take halflife time to reach mo
  testCalculateTimeToSaturation { mo: tagged $ Bar 2.0, pa: tagged $ Bar 3.0, pt: tagged $ Bar 1.0, tissueHalflife: tagged $ Minutes 1.0} (Just (Minutes 1.0))
  -- since mo is right in the middle of pa and pt, should take halflife time to reach mo
  testCalculateTimeToSaturation { mo: tagged $ Bar 2.0, pa: tagged $ Bar 1.0, pt: tagged $ Bar 3.0, tissueHalflife: tagged $ Minutes 1.0} (Just (Minutes 1.0))

  -- if mo < pa,  and pt > mo, we are not heading towards saturation
  testCalculateTimeToSaturation { mo: tagged $ Bar 1.0, pa: tagged $ Bar 3.0, pt: tagged $ Bar 2.0, tissueHalflife: tagged $ Minutes 1.0} Nothing
  testCalculateTimeToSaturation { mo: tagged $ Bar 1.0, pa: tagged $ Bar 2.0, pt: tagged $ Bar 3.0, tissueHalflife: tagged $ Minutes 1.0} Nothing


  -- testCalculateSafeAscentDepth
  log "testCalculateSafeAscentDepth"

  let -- mo and depth don't matter
      surfaceN2Pa = HA.calculatePa (tagged partialPercentage) zero
      tissueConf = HA.toTissueConf surfaceN2Pa (Minutes 27.0)-- mo = Bar 1.6
      depth = Meters 40.0
      -- if we are below the pa at a certain depth, our sad should be shallower than that depth
      pt1 = tagged $ Bar $ (extract tissueConf.mo) * (unwrap depth / 10.0 + 1.0) - 1.0
  assert' "calculateSafeAscentDepth failed" $ (HA.calculateSafeAscentDepth pt1 tissueConf) < depth

      -- if we are above the pa at a certain depth, our sad should be deeper than that depth
  let pt2 = tagged $ Bar $ (extract tissueConf.mo) * (unwrap depth / 10.0 + 1.0) + 1.0
  assert' "calculateSafeAscentDepth failed" $ (HA.calculateSafeAscentDepth pt2 tissueConf) > depth
