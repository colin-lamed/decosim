module Decosim.UI.DiveProfile
  ( drawProfile
  ) where

import Prelude
import Data.Array as A
import Data.Int as I
import Data.Either (Either(Right))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Minutes)
import Data.Traversable (for_)
import Effect (Effect)
import Graphics.Canvas (CanvasElement)
import Graphics.Canvas as C
import Math ((%), pi)

import Decosim.Sim (Sim)
import Decosim.Sim.HaldaneanModel as M
import Decosim.Sim.Types (Meters(..), toNumber)


drawProfile ∷ CanvasElement → Sim → Effect Unit
drawProfile canvas sim = do
  dim ← C.getCanvasDimensions canvas
  ctx ← C.getContext2D canvas


  let gridTopInset    = 5.0
      gridBottomInset = 15.0
      gridRightInset  = 20.0
      gridLeftInset   = 20.0
      yMax = dim.height - gridBottomInset
      xMax = dim.width  - gridRightInset

      scale ∷ Number → Number → Number → Number → Number
      scale value min max total =
        total * (value - min) / (max - min)

      getX ∷ Minutes → Number
      getX time = gridLeftInset + (scale (toNumber time) 0.0 (toNumber sim.maxViewableTime) (dim.width - gridLeftInset - gridRightInset))

      getY ∷ Meters → Number
      getY depth = gridTopInset + (scale (toNumber depth) 0.0 (toNumber sim.maxViewableDepth) (dim.height - gridBottomInset - gridTopInset))


  C.withContext ctx do

    gradient ← C.createLinearGradient ctx { x0: 0.0, y0: 0.0, x1: 0.0, y1: dim.height }
    void $ C.addColorStop gradient 0.0 "rgb(62, 87, 168)"
    void $ C.addColorStop gradient 1.0 "rgb(34, 48, 93)"
    void $ C.setGradientFillStyle ctx gradient
    void $ C.fillRect ctx { x     : 0.0
                          , y     : 0.0
                          , width : dim.width
                          , height: dim.height
                          }

  -- draw no-deco area
  when (not $ M.decoRequired sim.model) $
    C.withContext ctx do
      void $ C.setFillStyle ctx "rgba(143, 160, 214, 0.05)"
      C.fillPath ctx do
        void $ C.moveTo ctx (getX sim.time) (getY mempty)
        void $ C.lineTo ctx (getX sim.time) (getY (M.maxOperatingDepth sim.model.gasMix))
        for_ ( A.range 0 (I.round $ toNumber $ M.maxOperatingDepth sim.model.gasMix)
             # map I.toNumber
             # A.filter (\i → i % 5.0 == 0.0)
             # map Meters
             # A.reverse
             ) \depth → do
          let time = sim.time <> M.noDecoTimeAtDepth sim.model depth
          void $ C.lineTo ctx (getX (min time  sim.maxViewableTime ))
                              (getY (min depth sim.maxViewableDepth))

        void $ C.lineTo ctx (getX sim.maxViewableTime) (getY mempty)
        void $ C.closePath ctx

  -- draw grid
  C.withContext ctx do
    void $ C.setStrokeStyle ctx "rgb(222,222,222)"

    void $ C.strokeText ctx "0" 7.0 20.0
    void $ C.strokeText ctx (show $ toNumber sim.maxViewableDepth) 2.0 yMax
    void $ C.strokeText ctx "depth (m)" 2.0 (yMax / 2.0)

    void $ C.strokeText ctx "0" 20.0 (yMax + 10.0)
    void $ C.strokeText ctx (show (I.round $ toNumber sim.maxViewableTime)) (xMax - 10.0) (yMax + 10.0)
    void $ C.strokeText ctx "time (min)" (dim.width / 2.0) (yMax + 10.0)

   -- draw boundary
    C.strokePath ctx do
      void $ C.moveTo ctx gridLeftInset gridTopInset
      void $ C.lineTo ctx gridLeftInset yMax
      void $ C.lineTo ctx xMax          yMax
      void $ C.lineTo ctx xMax          gridTopInset
      void $ C.lineTo ctx gridLeftInset gridTopInset

  -- draw depth markings every 10 meters
  C.withContext ctx do
    void $ C.setStrokeStyle ctx "rgb(63,166,192)"

    for_ ( A.range 10 (I.round $ toNumber sim.maxViewableDepth)
         # map I.toNumber
         # A.filter (\i → i % 10.0 == 0.0)
         # map Meters
         ) \depth → do
      let y = getY depth
      void $ C.strokeText ctx (show $ toNumber depth) 2.0 y
      C.strokePath ctx do
        void $ C.moveTo ctx (gridLeftInset + 1.0) y
        void $ C.lineTo ctx (xMax          - 1.0) y



  -- draw ceiling and floor
  let drawHorizontalLine y =
        C.withContext ctx do
          void $ C.setStrokeStyle ctx "rgb(255, 0, 0)"
          C.strokePath ctx do
            void $ C.moveTo ctx (getX sim.time) y
            void $ C.lineTo ctx (xMax    - 1.0) y

  -- 1) SAD
  let sad = M.safeAscentDepth sim.model
  when (sad > mempty) $
    drawHorizontalLine (getY sad)

  -- 2) O2 floor
  let maxDepth = M.maxOperatingDepth sim.model.gasMix
  drawHorizontalLine (getY maxDepth)

  -- draw deco stops
  when (M.decoRequired sim.model) do
    C.withContext ctx do
      void $ C.setStrokeStyle ctx "rgb(255, 128, 0)"
      C.strokePath ctx do
        -- TODO start at time we would arrive at decoStopDepth (i.e. include ascent rate, and off-gassing during ascent)
        let drawStop t0 stop = do
              let t1 = t0 <> stop.time
              when (getX t0 < (xMax - 1.0)) do
                void $ C.moveTo ctx (getX t0)                    (getY stop.depth)
                void $ C.lineTo ctx (min (getX t1) (xMax - 1.0)) (getY stop.depth)
              pure t1

        case M.decoStops sim.model of
          Right dss → void $ foldM drawStop sim.time dss
          _         → pure unit

  -- draw dive profile
  C.withContext ctx do
    case A.head sim.historicData of
      Nothing           → pure unit
      Just historicData → do
        C.strokePath ctx do
          void $ C.arc ctx { x      : toNumber historicData.time  - 1.0
                           , y      : toNumber historicData.depth - 1.0
                           , radius : 2.0
                           , start  : 0.0
                           , end    : pi
                           }

    void $ C.beginPath ctx
    for_ sim.historicData \historicData → do
      let x = getX historicData.time
          y = getY historicData.depth
      -- TODO this is changing the whole line colour, need to draw individual paths
      void $ C.setStrokeStyle ctx (M.getGasMixColour historicData.gasMix)
      C.lineTo ctx x y
    void $ C.stroke ctx
