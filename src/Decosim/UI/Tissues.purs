module Decosim.UI.Tissues
  ( drawTissues
  , GasType(..)
  , gasName
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Array as A
import Data.Functor.Tagged (untagged)
import Data.Int as I
import Data.Newtype (unwrap)
import Data.Number.Format (toStringWith, fixed)
import Data.Traversable (for_)
import Graphics.Canvas (CANVAS, CanvasElement)
import Graphics.Canvas as C

import Decosim.Sim (Sim)
import Decosim.Sim.HaldaneanModel as M


data GasType = He | N2
derive instance eqGasType :: Eq GasType
instance showGasType :: Show GasType where
  show He = "He"
  show N2 = "N2"

gasName :: GasType -> String
gasName He = "Helium"
gasName N2 = "Nitrogen"



infoWith :: Number
infoWith    = 80.0

headerSpace :: Number
headerSpace = 20.0

paColour :: String
paColour = "rgb(55,166,199)"

moColour :: String
moColour = "MAGENTA"


drawTissues :: forall e. CanvasElement -> Sim -> GasType -> Eff (canvas :: CANVAS | e) Unit
drawTissues canvas sim gasType = do
  dim <- C.getCanvasDimensions canvas
  ctx <- C.getContext2D canvas

  let pa = case gasType of
            He -> untagged $ M.hePa sim.model
            N2 -> untagged $ M.n2Pa sim.model

      graphWidth         = dim.width - infoWith
      saturatedLineWidth = graphWidth * 2.0 / 3.0
      tissueSpacing      = (dim.height - headerSpace) / (I.toNumber $ A.length sim.model.tissues)


  C.withContext ctx $ do
    void $ C.setFillStyle "rgb(0,0,0)" ctx
    void $ C.fillRect ctx { x: 0.0
                          , y: 0.0
                          , w: dim.width
                          , h: dim.height
                          }


  C.withContext ctx $ do
    void $ C.setStrokeStyle paColour ctx
    void $ C.strokeText ctx (gasName gasType <> " Pa (bar): " <> toStringWith (fixed 3) (unwrap pa)) 5.0 12.0

  C.withContext ctx $ do
    void $ C.setStrokeStyle moColour ctx
    void $ C.strokeText ctx "MO" (graphWidth + 10.0) 12.0

  C.withContext ctx $ do
    void $ C.setStrokeStyle "rgb(255, 204, 102)" ctx
    void $ C.strokeText ctx "Pt" (graphWidth + 50.0) 12.0

  C.withContext ctx $ do
    void $ C.setStrokeStyle moColour ctx
    C.strokePath ctx $ do
      void $ C.moveTo ctx saturatedLineWidth headerSpace
      void $ C.lineTo ctx saturatedLineWidth dim.height


  for_ sim.model.tissues \tissue -> do
    let y = ((I.toNumber tissue.i + 0.5) * tissueSpacing) + headerSpace

        mo = case gasType of
               He -> untagged $ (M.heConf tissue).mo
               N2 -> untagged $ (M.n2Conf tissue).mo

        pt = case gasType of
               He -> untagged $ tissue.hePt
               N2 -> untagged $ tissue.n2Pt

        lineWidth    = min (unwrap pt / unwrap mo * saturatedLineWidth) graphWidth
        paNormalised = min (unwrap pa / unwrap mo * saturatedLineWidth) graphWidth

        tissueColour = "rgb(245," <> show (240 - tissue.i * 240 / A.length sim.model.tissues) <> ",45)"

    C.withContext ctx $ do
      void $ C.setStrokeStyle "rgb(191, 191, 191)" ctx
      C.strokePath ctx $ do
        void $ C.moveTo ctx 0.0                    y
        void $ C.lineTo ctx (dim.width - infoWith) y

    C.withContext ctx $ do
      void $ C.setFillStyle tissueColour ctx
      C.fillPath ctx $ do
        void $ C.moveTo ctx 0.0       (y - 2.0)
        void $ C.lineTo ctx lineWidth (y - 2.0)
        void $ C.lineTo ctx lineWidth (y + 2.0)
        void $ C.lineTo ctx 0.0       (y + 2.0)

    C.withContext ctx $ do
      void $ C.setFillStyle paColour ctx
      C.fillPath ctx $ do
        void $ C.moveTo ctx (paNormalised - 1.0) (y - 3.0)
        void $ C.lineTo ctx (paNormalised - 1.0) (y + 3.0)
        void $ C.lineTo ctx (paNormalised + 1.0) (y + 3.0)
        void $ C.lineTo ctx (paNormalised + 1.0) (y - 3.0)

    C.withContext ctx $ do
      void $ C.setStrokeStyle moColour ctx
      void $ C.strokeText ctx (toStringWith (fixed 2) (unwrap mo)) (graphWidth + 10.0) (y + 5.0)

    C.withContext ctx $ do
      void $ C.setStrokeStyle tissueColour ctx
      void $ C.strokeText ctx (toStringWith (fixed 2) (unwrap pt)) (graphWidth + 50.0) (y + 5.0)
