module Decosim.Sim.HaldaneadModelSpec where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array ((..))
import Data.Int as I
import Data.Foldable (for_)
import Data.Functor.Tagged (tagged, untagged)
import Data.Record (equal)
import Data.Record.ShowRecord (showRecord)
import Data.Time.Duration (Minutes(Minutes))
import Test.Assert (ASSERT, assert')

import Decosim.Sim.Types (Bar(Bar), Meters(Meters))
import Decosim.Sim.HaldaneanModel
import Decosim.Sim.HaldaneanAlgorithm as HA


spec :: forall e. Eff (console :: CONSOLE, assert :: ASSERT | e) Unit
spec = do
  log "\nHaldaneadModelSpec:"

  assert' "testPercentHe failed" $ percentHe Air == tagged 0.0

  assert' "test n2Confs failed " $ n2Confs `at` 0 `equal`
    { a: tagged 1.2599210498948732 , b: tagged 0.5049999999999999, halflife: tagged $ Minutes 4.0, mo: tagged $ Bar 2.735578475637448, k: tagged 0.17328679513998632   }

  assert' "test n2Confs failed " $ map showRecord n2Confs == map showRecord
    [{ a: tagged 1.2599210498948732 , b: tagged 0.5049999999999999, halflife: tagged $ Minutes   4.0, mo: tagged $ Bar 2.735578475637448 , k: tagged 0.17328679513998632   }
    ,{ a: tagged 1.0                , b: tagged 0.6514466094067262, halflife: tagged $ Minutes   8.0, mo: tagged $ Bar 2.143926438850701 , k: tagged 0.08664339756999316   }
    ,{ a: tagged 0.8617738760127536 , b: tagged 0.7221572875253809, halflife: tagged $ Minutes  12.5, mo: tagged $ Bar 1.8936917322371278, k: tagged 0.055451774444795626  }
    ,{ a: tagged 0.7562047822671437 , b: tagged 0.7725047225123614, halflife: tagged $ Minutes  18.5, mo: tagged $ Bar 1.7208681406689115, k: tagged 0.037467415165402446  }
    ,{ a: tagged 0.6666666666666667 , b: tagged 0.8125499102701246, halflife: tagged $ Minutes  27.0, mo: tagged $ Bar 1.5837881758577304, k: tagged 0.025672117798516494  }
    ,{ a: tagged 0.5933310427913479 , b: tagged 0.8434151598249001, halflife: tagged $ Minutes  38.3, mo: tagged $ Bar 1.476889977343474 , k: tagged 0.01809783761253121   }
    ,{ a: tagged 0.528157420362247  , b: tagged 0.869293676412961 , halflife: tagged $ Minutes  54.3, mo: tagged $ Bar 1.3854131674361356, k: tagged 0.012765141446776157  }
    ,{ a: tagged 0.47011028632085433, b: tagged 0.8910394235403619, halflife: tagged $ Minutes  77.0, mo: tagged $ Bar 1.3064447742373073, k: tagged 0.009001911435843446  }
    ,{ a: tagged 0.41868541237233853, b: tagged 0.9092173714778847, halflife: tagged $ Minutes 109.0, mo: tagged $ Bar 1.2382990970390817, k: tagged 0.006359148445504085  }
    ,{ a: tagged 0.37982106198575216, b: tagged 0.9222394111397632, halflife: tagged $ Minutes 146.0, mo: tagged $ Bar 1.1878617843823638, k: tagged 0.004747583428492776  }
    ,{ a: tagged 0.34974334561894677, b: tagged 0.9318727575872868, halflife: tagged $ Minutes 187.0, mo: tagged $ Bar 1.149430850090498 , k: tagged 0.0037066694147590657 }
    ,{ a: tagged 0.3222780263598589 , b: tagged 0.9403153772646848, halflife: tagged $ Minutes 239.0, mo: tagged $ Bar 1.1147855382201435, k: tagged 0.002900197408200608  }
    ,{ a: tagged 0.297118743106386  , b: tagged 0.947740166568613 , halflife: tagged $ Minutes 305.0, mo: tagged $ Bar 1.0834175898652985, k: tagged 0.002272613706753919  }
    ,{ a: tagged 0.2737422252554831 , b: tagged 0.9543630316458166, halflife: tagged $ Minutes 390.0, mo: tagged $ Bar 1.0545844994107139, k: tagged 0.0017773004629742187 }
    ,{ a: tagged 0.2523210876661755 , b: tagged 0.9601889285051778, halflife: tagged $ Minutes 498.0, mo: tagged $ Bar 1.0284256415482311, k: tagged 0.0013918618083533039 }
    ,{ a: tagged 0.23268698225807122, b: tagged 0.9653162104933727, halflife: tagged $ Minutes 635.0, mo: tagged $ Bar 1.0046692528335597, k: tagged 0.0010915703630865281 }
    ]

  assert' "test heConfs failed " $ map showRecord heConfs == map showRecord
    [{ a: tagged 1.7471609294725978 , b: tagged 0.18850341907227375, halflife: tagged (Minutes   1.5), mo: tagged (Bar 1.7471609294725978 ), k: tagged 0.46209812037329684   }
    ,{ a: tagged 1.3867225487012695 , b: tagged 0.42764973081037405, halflife: tagged (Minutes   3.0), mo: tagged (Bar 1.3867225487012695 ), k: tagged 0.23104906018664842   }
    ,{ a: tagged 1.193980891397839  , b: tagged 0.5437343959855574 , halflife: tagged (Minutes   4.7), mo: tagged (Bar 1.193980891397839  ), k: tagged 0.1474781235233926    }
    ,{ a: tagged 1.0455159171494204 , b: tagged 0.6270355269907727 , halflife: tagged (Minutes   7.0), mo: tagged (Bar 1.0455159171494204 ), k: tagged 0.09902102579427789   }
    ,{ a: tagged 0.9222102361750056 , b: tagged 0.6918878544574252 , halflife: tagged (Minutes  10.2), mo: tagged (Bar 0.9222102361750056 ), k: tagged 0.06795560593724954   }
    ,{ a: tagged 0.8201765126313855 , b: tagged 0.7423871342805548 , halflife: tagged (Minutes  14.5), mo: tagged (Bar 0.8201765126313855 ), k: tagged 0.04780325383172036   }
    ,{ a: tagged 0.7307666321829711 , b: tagged 0.7841369478503069 , halflife: tagged (Minutes  20.5), mo: tagged (Bar 0.7307666321829711 ), k: tagged 0.03381205758829001   }
    ,{ a: tagged 0.6502279983967278 , b: tagged 0.8196240005599837 , halflife: tagged (Minutes  29.1), mo: tagged (Bar 0.6502279983967278 ), k: tagged 0.02381949074089159   }
    ,{ a: tagged 0.5795390729040895 , b: tagged 0.8490163462304173 , halflife: tagged (Minutes  41.1), mo: tagged (Bar 0.5795390729040895 ), k: tagged 0.01686489490413492   }
    ,{ a: tagged 0.5255888330381511 , b: tagged 0.8702824423964018 , halflife: tagged (Minutes  55.1), mo: tagged (Bar 0.5255888330381511 ), k: tagged 0.012579803639926411  }
    ,{ a: tagged 0.4839068412215842 , b: tagged 0.8859861102685551 , halflife: tagged (Minutes  70.6), mo: tagged (Bar 0.4839068412215842 ), k: tagged 0.009817948733143702  }
    ,{ a: tagged 0.44595853812463876, b: tagged 0.8997076712143346 , halflife: tagged (Minutes  90.2), mo: tagged (Bar 0.44595853812463876), k: tagged 0.007684558542793185  }
    ,{ a: tagged 0.4111543484341618 , b: tagged 0.9117900364408941 , halflife: tagged (Minutes 115.1), mo: tagged (Bar 0.4111543484341618 ), k: tagged 0.0060221301525625135 }
    ,{ a: tagged 0.3787861220823229 , b: tagged 0.9225774408255265 , halflife: tagged (Minutes 147.2), mo: tagged (Bar 0.3787861220823229 ), k: tagged 0.00470888030271702   }
    ,{ a: tagged 0.349184053600252  , b: tagged 0.9320480995691212 , halflife: tagged (Minutes 187.9), mo: tagged (Bar 0.349184053600252  ), k: tagged 0.003688915277061976  }
    ,{ a: tagged 0.3220087881172739 , b: tagged 0.9403964187950271 , halflife: tagged (Minutes 239.6), mo: tagged (Bar 0.3220087881172739 ), k: tagged 0.002892934810350356  }
    ]

  let model = initModel
      expected = { gasMix  : Air
                 , depth   : zero
                 , tissues : [ { i: 0,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 1,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 2,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 3,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 4,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 5,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 6,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 7,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 8,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 9,  n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 10, n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 11, n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 12, n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 13, n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 14, n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             , { i: 15, n2Pt: tagged $ Bar 0.7452070000000001, hePt: tagged $ zero, deco: TDNoDecoTime zero }
                             ]
                 }
  log "assert initial model"
  assert' (showModel model <> " did not equal " <> showModel expected) $ showModel model == showModel expected


  -- testCalculateDecoStopDepth
  log "testCalculateDecoStopDepth"
  let testCalculateDecoStopDepth1 i =
        let sad = Meters $ 3.0 * I.toNumber i
            dsd = calculateDecoStopDepth sad
        in assert' "sad should == dsd" $ 3 * i == dsd
  for_ (0..10) testCalculateDecoStopDepth1
  let testCalculateDecoStopDepth2 i =
        let sad = Meters $ 3.0 * I.toNumber i + 0.1
            dsd = calculateDecoStopDepth sad
        in assert' "dsd should be greater than sad" $ 3 * (i + 1) == dsd
  for_ (0..10) testCalculateDecoStopDepth2
  let testCalculateDecoStopDepth3 i =
        let sad = Meters $ 3.0 * (I.toNumber i + 1.0) - 0.1
            dsd = calculateDecoStopDepth sad
        in assert' "deco stop depth should be rounded up" $ 3 * (i + 1) == dsd
  for_ (0..10) testCalculateDecoStopDepth3



  -- testGetN2Pa
  log "testGetN2Pa"
  -- tolerance for water vapour
  let approxEq :: Bar -> Bar -> Boolean
      approxEq b1 b2 =
        b1 - b2 < Bar 0.1
  -- should be double at 10m than at the surface
  assert' "testGetN2Pa failed" $ (untagged $ n2Pa $ updateModel (Meters 10.0) (Minutes 1.0) model) `approxEq` (Bar $ untagged (percentN2 model.gasMix) * 2.0)
  -- should be triple at 20m than at the surface
  assert' "testGetN2Pa failed" $ (untagged $ n2Pa $ updateModel (Meters 20.0) (Minutes 1.0) model) `approxEq` (Bar $ untagged (percentN2 model.gasMix) * 3.0)
  -- should be 4 times 30m than at the surface
  assert' "testGetN2Pa failed" $ (untagged $ n2Pa $ updateModel (Meters 30.0) (Minutes 1.0) model) `approxEq` (Bar $ untagged (percentN2 model.gasMix) * 4.0)


  -- testGetNoDecoTime
  log "testGetNoDecoTime"
  -- we can't enter no deco at 3m (Pa is lower than lowest Mo[i])
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 3.0 ) (Minutes 90.0) model) == HA.maxtime
  -- at 10 meters it is initially very large
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 10.0) (Minutes 1.0) model) == HA.maxtime
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 15.0) (Minutes 2.0) model) <  HA.maxtime
  -- we should have entered deco now (50m for 5min), so no deco is 0
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 50.0) (Minutes 5.0) model) == zero

  -- testGetSafeAscentDepth
  log "testGetSafeAscentDepth"
  -- haven't entered deco here, so should be able to go to surface
  assert' "testGetSafeAscentDepth failed" $ safeAscentDepth (updateModel (Meters 10.0) (Minutes 1.0) model) == zero
  -- have entered deco here, so should not be able to go to surface
  assert' "testGetSafeAscentDepth failed" $ safeAscentDepth (updateModel (Meters 50.0) (Minutes 5.0) model) > zero

  -- testGetNoDecoTimeAtDepth
  log "testGetNoDecoTimeAtDepth"

  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 3.0 ) == HA.maxtime

  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 15.0) > zero
  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 15.0) < HA.maxtime

  -- We will enter deco quickly but not yet
  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 50.0) > zero
  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 50.0) < HA.maxtime

  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth (updateModel (Meters 30.0) (Minutes 30.0) model) (Meters 50.0) == zero


  -- test maxOperatingDepth
  log "test maxOperatingDepth"
  assert' "test maxOperatingDepth Air failed" $ maxOperatingDepth Air      > Meters 55.0
  assert' "test maxOperatingDepth Air failed" $ maxOperatingDepth NitroxI  < maxOperatingDepth Air
  assert' "test maxOperatingDepth Air failed" $ maxOperatingDepth NitroxII < maxOperatingDepth NitroxI
  assert' "test maxOperatingDepth Air failed" $ maxOperatingDepth Oxygen   < maxOperatingDepth NitroxII
  assert' "test maxOperatingDepth Air failed" $ maxOperatingDepth Oxygen   < Meters 10.0

  -- testIsDecoRequired
  log "testIsDecoRequired"
  assert' "testIsDecoRequired failed" $ decoRequired (updateModel (Meters 10.0) (Minutes 30.0) model) == false
  assert' "testIsDecoRequired failed" $ decoRequired (updateModel (Meters 30.0) (Minutes 30.0) model) == true
