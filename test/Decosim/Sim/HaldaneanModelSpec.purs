module Decosim.Sim.HaldaneadModelSpec where

import Prelude
import Data.Array ((..))
import Data.Foldable (for_)
import Data.Int as I
import Data.Tagged (tag, untag)
import Data.Time.Duration (Minutes(Minutes))
import Effect (Effect)
import Effect.Console (log)
import Test.Assert (assert')

import Decosim.Sim.Types (Bar(Bar), Meters(Meters), toNumber)
import Decosim.Sim.HaldaneanModel
import Decosim.Sim.HaldaneanAlgorithm as HA


spec ∷ Effect Unit
spec = do
  log "\nHaldaneadModelSpec:"

  assert' "testPercentHe failed" $ percentHe Air == tag 0.0

  assert' "test n2Confs failed " $ n2Confs `at` 0 ==
    { a: tag 1.2599210498948732 , b: tag 0.5049999999999999, halflife: tag $ Minutes 4.0, mo: tag $ Bar 2.735578475637448, k: tag 0.17328679513998632   }

  assert' "test n2Confs failed " $ map show n2Confs == map show
    [{ a: tag 1.2599210498948732 , b: tag 0.5049999999999999, halflife: tag $ Minutes   4.0, mo: tag $ Bar 2.735578475637448 , k: tag 0.17328679513998632   }
    ,{ a: tag 1.0                , b: tag 0.6514466094067262, halflife: tag $ Minutes   8.0, mo: tag $ Bar 2.143926438850701 , k: tag 0.08664339756999316   }
    ,{ a: tag 0.8617738760127536 , b: tag 0.7221572875253809, halflife: tag $ Minutes  12.5, mo: tag $ Bar 1.8936917322371278, k: tag 0.055451774444795626  }
    ,{ a: tag 0.7562047822671437 , b: tag 0.7725047225123614, halflife: tag $ Minutes  18.5, mo: tag $ Bar 1.7208681406689115, k: tag 0.037467415165402446  }
    ,{ a: tag 0.6666666666666667 , b: tag 0.8125499102701246, halflife: tag $ Minutes  27.0, mo: tag $ Bar 1.5837881758577304, k: tag 0.025672117798516494  }
    ,{ a: tag 0.5933310427913479 , b: tag 0.8434151598249001, halflife: tag $ Minutes  38.3, mo: tag $ Bar 1.476889977343474 , k: tag 0.01809783761253121   }
    ,{ a: tag 0.528157420362247  , b: tag 0.869293676412961 , halflife: tag $ Minutes  54.3, mo: tag $ Bar 1.3854131674361356, k: tag 0.012765141446776157  }
    ,{ a: tag 0.47011028632085433, b: tag 0.8910394235403619, halflife: tag $ Minutes  77.0, mo: tag $ Bar 1.3064447742373073, k: tag 0.009001911435843446  }
    ,{ a: tag 0.41868541237233853, b: tag 0.9092173714778847, halflife: tag $ Minutes 109.0, mo: tag $ Bar 1.2382990970390817, k: tag 0.006359148445504085  }
    ,{ a: tag 0.37982106198575216, b: tag 0.9222394111397632, halflife: tag $ Minutes 146.0, mo: tag $ Bar 1.1878617843823638, k: tag 0.004747583428492776  }
    ,{ a: tag 0.34974334561894677, b: tag 0.9318727575872868, halflife: tag $ Minutes 187.0, mo: tag $ Bar 1.149430850090498 , k: tag 0.0037066694147590657 }
    ,{ a: tag 0.3222780263598589 , b: tag 0.9403153772646848, halflife: tag $ Minutes 239.0, mo: tag $ Bar 1.1147855382201435, k: tag 0.002900197408200608  }
    ,{ a: tag 0.297118743106386  , b: tag 0.947740166568613 , halflife: tag $ Minutes 305.0, mo: tag $ Bar 1.0834175898652985, k: tag 0.002272613706753919  }
    ,{ a: tag 0.2737422252554831 , b: tag 0.9543630316458166, halflife: tag $ Minutes 390.0, mo: tag $ Bar 1.0545844994107139, k: tag 0.0017773004629742187 }
    ,{ a: tag 0.2523210876661755 , b: tag 0.9601889285051778, halflife: tag $ Minutes 498.0, mo: tag $ Bar 1.0284256415482311, k: tag 0.0013918618083533039 }
    ,{ a: tag 0.23268698225807122, b: tag 0.9653162104933727, halflife: tag $ Minutes 635.0, mo: tag $ Bar 1.0046692528335597, k: tag 0.0010915703630865281 }
    ]

  assert' "test heConfs failed " $ map show heConfs == map show
    [{ a: tag 1.7471609294725978 , b: tag 0.18850341907227386, halflife: tag (Minutes   1.5), mo: tag (Bar 1.7471609294725978 ), k: tag 0.46209812037329684   }
    ,{ a: tag 1.3867225487012695 , b: tag 0.42764973081037416, halflife: tag (Minutes   3.0), mo: tag (Bar 1.3867225487012695 ), k: tag 0.23104906018664842   }
    ,{ a: tag 1.193980891397839  , b: tag 0.5437343959855574 , halflife: tag (Minutes   4.7), mo: tag (Bar 1.193980891397839  ), k: tag 0.1474781235233926    }
    ,{ a: tag 1.0455159171494204 , b: tag 0.6270355269907726 , halflife: tag (Minutes   7.0), mo: tag (Bar 1.0455159171494204 ), k: tag 0.09902102579427789   }
    ,{ a: tag 0.9222102361750056 , b: tag 0.6918878544574252 , halflife: tag (Minutes  10.2), mo: tag (Bar 0.9222102361750056 ), k: tag 0.06795560593724954   }
    ,{ a: tag 0.8201765126313855 , b: tag 0.7423871342805548 , halflife: tag (Minutes  14.5), mo: tag (Bar 0.8201765126313855 ), k: tag 0.04780325383172036   }
    ,{ a: tag 0.7307666321829711 , b: tag 0.7841369478503069 , halflife: tag (Minutes  20.5), mo: tag (Bar 0.7307666321829711 ), k: tag 0.03381205758829001   }
    ,{ a: tag 0.6502279983967278 , b: tag 0.8196240005599837 , halflife: tag (Minutes  29.1), mo: tag (Bar 0.6502279983967278 ), k: tag 0.02381949074089159   }
    ,{ a: tag 0.5795390729040895 , b: tag 0.8490163462304173 , halflife: tag (Minutes  41.1), mo: tag (Bar 0.5795390729040895 ), k: tag 0.01686489490413492   }
    ,{ a: tag 0.5255888330381511 , b: tag 0.8702824423964018 , halflife: tag (Minutes  55.1), mo: tag (Bar 0.5255888330381511 ), k: tag 0.012579803639926411  }
    ,{ a: tag 0.4839068412215842 , b: tag 0.8859861102685551 , halflife: tag (Minutes  70.6), mo: tag (Bar 0.4839068412215842 ), k: tag 0.009817948733143702  }
    ,{ a: tag 0.44595853812463876, b: tag 0.8997076712143346 , halflife: tag (Minutes  90.2), mo: tag (Bar 0.44595853812463876), k: tag 0.007684558542793185  }
    ,{ a: tag 0.4111543484341618 , b: tag 0.9117900364408941 , halflife: tag (Minutes 115.1), mo: tag (Bar 0.4111543484341618 ), k: tag 0.0060221301525625135 }
    ,{ a: tag 0.3787861220823229 , b: tag 0.9225774408255265 , halflife: tag (Minutes 147.2), mo: tag (Bar 0.3787861220823229 ), k: tag 0.00470888030271702   }
    ,{ a: tag 0.349184053600252  , b: tag 0.9320480995691212 , halflife: tag (Minutes 187.9), mo: tag (Bar 0.349184053600252  ), k: tag 0.003688915277061976  }
    ,{ a: tag 0.3220087881172739 , b: tag 0.9403964187950271 , halflife: tag (Minutes 239.6), mo: tag (Bar 0.3220087881172739 ), k: tag 0.002892934810350356  }
    ]

  let model = initModel
      expected = { gasMix  : Air
                 , depth   : mempty
                 , tissues : [ { i: 0,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 1,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 2,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 3,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 4,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 5,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 6,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 7,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 8,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 9,  n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 10, n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 11, n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 12, n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 13, n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 14, n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
                             , { i: 15, n2Pt: tag $ Bar 0.7452070000000001, hePt: tag mempty, deco: TDNoDecoTime mempty }
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
  let approxEq ∷ Bar → Bar → Boolean
      approxEq b1 b2 =
        toNumber b1 - toNumber b2 < 0.1
  -- should be double at 10m than at the surface
  assert' "testGetN2Pa failed" $ (untag $ n2Pa $ updateModel (Meters 10.0) (Minutes 1.0) model) `approxEq` (Bar $ untag (percentN2 model.gasMix) * 2.0)
  -- should be triple at 20m than at the surface
  assert' "testGetN2Pa failed" $ (untag $ n2Pa $ updateModel (Meters 20.0) (Minutes 1.0) model) `approxEq` (Bar $ untag (percentN2 model.gasMix) * 3.0)
  -- should be 4 times 30m than at the surface
  assert' "testGetN2Pa failed" $ (untag $ n2Pa $ updateModel (Meters 30.0) (Minutes 1.0) model) `approxEq` (Bar $ untag (percentN2 model.gasMix) * 4.0)


  -- testGetNoDecoTime
  log "testGetNoDecoTime"
  -- we can't enter no deco at 3m (Pa is lower than lowest Mo[i])
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 3.0 ) (Minutes 90.0) model) == HA.maxtime
  -- at 10 meters it is initially very large
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 10.0) (Minutes 1.0) model) == HA.maxtime
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 15.0) (Minutes 2.0) model) <  HA.maxtime
  -- we should have entered deco now (50m for 5min), so no deco is 0
  assert' "testGetNoDecoTime failed" $ noDecoTime (updateModel (Meters 50.0) (Minutes 5.0) model) == mempty

  -- testGetSafeAscentDepth
  log "testGetSafeAscentDepth"
  -- haven't entered deco here, so should be able to go to surface
  assert' "testGetSafeAscentDepth failed" $ safeAscentDepth (updateModel (Meters 10.0) (Minutes 1.0) model) == mempty
  -- have entered deco here, so should not be able to go to surface
  assert' "testGetSafeAscentDepth failed" $ safeAscentDepth (updateModel (Meters 50.0) (Minutes 5.0) model) > mempty

  -- testGetNoDecoTimeAtDepth
  log "testGetNoDecoTimeAtDepth"

  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 3.0 ) == HA.maxtime

  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 15.0) > mempty
  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 15.0) < HA.maxtime

  -- We will enter deco quickly but not yet
  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 50.0) > mempty
  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth model (Meters 50.0) < HA.maxtime

  assert' "testGetNoDecoTimeAtDepth failed" $ noDecoTimeAtDepth (updateModel (Meters 30.0) (Minutes 30.0) model) (Meters 50.0) == mempty


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
