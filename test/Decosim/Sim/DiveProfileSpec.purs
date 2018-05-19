module Decosim.Sim.DiveProfileSpec
  ( airSpec
  , trimixIISpec
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.List as L
import Data.Array ((..))
import Data.Array as A
import Data.Int as I
import Data.Either (Either(Left,Right))
import Data.Foldable (for_)
import Data.Functor.Tagged (Tagged, tagged, untagged)
import Data.Maybe (Maybe(Just,Nothing),fromJust)
import Data.Newtype (over2, unwrap)
import Data.Record.ShowRecord (showRecord)
import Data.Time.Duration (Minutes(..))
import Data.Tuple (Tuple(Tuple))
import Math as Math
import Test.Assert
import Partial.Unsafe (unsafePartial)

import Decosim.Sim.Types (Bar(Bar), Meters(Meters))
import Decosim.Sim.HaldaneanModel
import Decosim.Sim.HaldaneanAlgorithm as HA


type Test = Eff (console :: CONSOLE, assert :: ASSERT)

n2Conf = HA.toTissueConf (HA.calculatePa (tagged 0.79) zero) (Minutes 18.5)
heConf = HA.toTissueConf (HA.calculatePa (tagged 0.0 ) zero) (Minutes 7.0)
tissueI = 3


airSpec :: Test Unit
airSpec = do
  log "\nDiveProfileSpec - Air:"

  -- dive to 60.0 m (no deco)
  let gasMix  = Air
      model0  = initModel { gasMix = gasMix }
      model1  = updateModel (Meters 60.0) zero model0
      tissue1 = model1.tissues `at` tissueI

      -- no deco time here
      noDecoTime = HA.noDecoTime n2Conf heConf tissue1.n2Pt (n2Pa model1) tissue1.hePt (hePa model1)

  assertIsNoDecoTime model1 noDecoTime

  -- stay at 60.0 m for 20 mins (deco)
  let model2 = updateModel (Meters 60.0) (Minutes 20.0) model1
      tissue2 = model2.tissues `at` tissueI
      dsd    = case tissue2.deco of
                 TDDecoStop m -> m
                 _            -> zero
      dst    = HA.decoStopTime dsd (percentN2 gasMix) (percentHe gasMix) n2Conf heConf tissue2.n2Pt tissue2.hePt

  assertIsAscentCeiling model2 dsd
  assertIsDecoStopTime model2 dsd dst

  case decoStops model2 of
    Left msg -> assert' msg false
    Right dss -> assertStops model2 dss



trimixIISpec :: Test Unit
trimixIISpec = do
  log "\nDiveProfileSpec - TrimixII:"
  log $ "heConf=" <> showRecord heConf

  -- dive to 60.0 m (no deco)
  let gasMix  = TrimixII
      model0  = initModel { gasMix = gasMix }
      model1  = updateModel (Meters 60.0) zero model0
      tissue1 = model1.tissues `at` tissueI

      -- no deco time here
      noDecoTime = HA.noDecoTime n2Conf heConf tissue1.n2Pt (n2Pa model1) tissue1.hePt (hePa model1)

  assertIsNoDecoTime model1 noDecoTime

  -- stay at 60.0 m for 20 mins (deco)
  let model2 = updateModel (Meters 60.0) (Minutes 20.0) model1
      tissue2 = model2.tissues `at` tissueI
      dsd    = case tissue2.deco of
                 TDDecoStop m -> m
                 _            -> zero
      dst    = HA.decoStopTime dsd (percentN2 gasMix) (percentHe gasMix) n2Conf heConf tissue2.n2Pt tissue2.hePt
  log $ "after 20min @ 60m: dsd: " <> show dsd <> ", dst: " <> show dst


  -- log $ "deco stops: " <> show (map showRecord $ decoStops model2)
  assertIsAscentCeiling model2 dsd
  assertIsDecoStopTime model2 dsd dst

  case decoStops model2 of
    Left msg -> assert' msg false
    Right dss -> assertStops model2 dss


assertIsNoDecoTime :: Model -> Minutes -> Test Unit
assertIsNoDecoTime model noDecoTime = do
  -- if we stay for no-deco-time, sad should be approx zero
  let model1  = updateModel (Meters 60.0) noDecoTime model
      tissue1 = model1.tissues `at` tissueI
      sadN2  = HA.calculateSafeAscentDepth tissue1.n2Pt n2Conf
      sadHe  = HA.calculateSafeAscentDepth tissue1.hePt heConf
      sad    = HA.ascentCeiling tissue1.n2Pt tissue1.hePt n2Conf heConf

  assert' ("Either sadN2 or sadHe should be zero. were sadN2:" <> show sadN2 <> ", sadHe: " <> show sadHe) (
    Math.abs (unwrap sadN2) < 0.0000000001 || Math.abs (unwrap sadHe) < 0.0000000001
  )
  assert' ("sad should be zero. was: " <> show sad) (Math.abs (unwrap sad) < 0.0000000001)


assertIsAscentCeiling :: Model -> Meters -> Test Unit
assertIsAscentCeiling model dsd = do
  let model1  = updateModel dsd zero model
      tissue1 = model1.tissues `at` tissueI
      maxTolerableN2Pt :: Bar
      maxTolerableN2Pt = over2 Bar (*) (untagged n2Conf.mo) (HA.depthToBar dsd)
      maxTolerableHePt :: Bar
      maxTolerableHePt = over2 Bar (*) (untagged heConf.mo) (HA.depthToBar dsd)

  -- pt or limiting gas should be max tolerable, the other should be tolerable
  assert' ("n2Pt " <> show tissue1.n2Pt <> " should be max tolerable " <> show maxTolerableN2Pt <> " or he2Pt " <> show tissue1.hePt <> " should be max tolerable " <> show maxTolerableHePt) (
    untagged tissue1.n2Pt == maxTolerableN2Pt || untagged tissue1.hePt == maxTolerableHePt
  )
  when (untagged tissue1.n2Pt == maxTolerableN2Pt) $
    assert' ("hePt " <> show tissue1.hePt <> " should be tolerable " <> show maxTolerableHePt <> " when n2Pt at max Tolerable") (
      untagged tissue1.hePt <= maxTolerableHePt
    )
  when (untagged tissue1.hePt == maxTolerableHePt) $
    assert' ("n2Pt " <> show tissue1.n2Pt <> " should be tolerable " <> show maxTolerableN2Pt <> " when hePt at max Tolerable") (
      untagged tissue1.n2Pt <= maxTolerableN2Pt
    )


assertIsDecoStopTime :: Model -> Meters -> Minutes  -> Test Unit
assertIsDecoStopTime model dsd dst = do
  log $ "assertIsDecoStopTime dsd:" <> show dsd <> ", dst:" <> show dst

  let tissue = (updateModel dsd dst model).tissues `at` tissueI
      sadN2    = HA.calculateSafeAscentDepth tissue.n2Pt n2Conf
      sadHe    = HA.calculateSafeAscentDepth tissue.hePt n2Conf
      sad      = HA.ascentCeiling tissue.n2Pt tissue.hePt n2Conf heConf

      nextStop = dsd - HA.stopDelta
      n2PtTolerableAtNextStop = untagged n2Conf.mo * HA.depthToBar nextStop
      hePtTolerableAtNextStop = untagged heConf.mo * HA.depthToBar nextStop

      -- since we are rounding time up, following is not true: tissue.n2Pt == ptTolerableAtNextStop
      -- check that if we stopped for less, we would have exceeded tolerable pt
      earlier = dst - Minutes 1.0
      tissueEarlier = (updateModel dsd earlier model).tissues `at` tissueI
      sadN2Earlier  = HA.calculateSafeAscentDepth tissueEarlier.n2Pt n2Conf
      sadHeEarlier  = HA.calculateSafeAscentDepth tissueEarlier.hePt heConf
      sadEarlier    = HA.ascentCeiling tissueEarlier.n2Pt tissueEarlier.hePt n2Conf heConf

  assert' ("n2Pt " <> show tissue.n2Pt <> " should be tolerable at next stop (< " <> show n2PtTolerableAtNextStop <> ")") (untagged tissue.n2Pt <= n2PtTolerableAtNextStop)
  assert' ("hePt " <> show tissue.hePt <> " should be tolerable at next stop (< " <> show hePtTolerableAtNextStop <> ")") (untagged tissue.hePt <= hePtTolerableAtNextStop)
  assert' "either n2Pt or hePt should not be tolerable at next stop if we left stop early" (
    untagged tissueEarlier.n2Pt > n2PtTolerableAtNextStop || untagged tissueEarlier.hePt > hePtTolerableAtNextStop
  )

  assert' ("either sadN2 or sadHe should be <= nextStop (" <> show nextStop <> "). sadN2: " <> show sadN2 <> ", sadHe: " <> show sadHe) (
    sadN2 <= nextStop || sadHe <= nextStop
  )
  assert' ("sad should be <= nextStop (" <> show nextStop <> "). was: " <> show sad) (sad <= nextStop)

  assert' ("either earlier sadN2 or sadHe should have been > nextStop (" <> show nextStop <> "). sadN2Earlier: " <> show sadN2Earlier <> ", sadHeEarlier: " <> show sadHeEarlier) (
    sadN2Earlier > nextStop || sadHeEarlier > nextStop
  )
  assert' ("earlier sad should have been > nextStop (" <> show nextStop <> "). was: " <> show sad) (sadEarlier > nextStop)


assertStops :: Model -> Array DecoStop -> Test Unit
assertStops model stops =
  case Tuple <$> A.head stops <*> A.tail stops of
    Nothing -> do
      log $ "decoStop': 0m"
      model1 <- assertDeco model zero
      pure unit
    Just (Tuple stop nextStops) -> do
      log $ "decoStop': " <> showRecord stop
      model1 <- assertDeco model stop.depth
      let nextModel = updateModel stop.depth stop.time $ (updateModel stop.depth zero model1) { gasMix = stop.gasMix }
      assertStops nextModel nextStops

assertDeco :: Model -> Meters -> Test Model
assertDeco model depth = do
  let model1  = updateModel depth zero model
      tissue1 = model1.tissues `at` tissueI
      maxTolerablePt :: Bar
      maxTolerablePt = over2 Bar (*) (untagged n2Conf.mo) (HA.depthToBar depth)
  assert' ("at " <> show depth <> " n2Pt " <> show tissue1.n2Pt <> " should be tolerable " <> show maxTolerablePt) (untagged tissue1.n2Pt <= maxTolerablePt)
  pure model1
