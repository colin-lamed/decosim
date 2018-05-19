module Decosim.UI
  ( panel
  ) where

import Prelude
import React

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Ref as R
import Control.Monad.Eff.Timer (TIMER, setTimeout)
import DOM (DOM)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.HTML.Event.EventTypes (resize)
import DOM.HTML.HTMLElement (offsetHeight)
import DOM.HTML.Types (Window, windowToEventTarget)
import DOM.HTML.Window (innerWidth, innerHeight, requestAnimationFrame)
import Data.DateTime.Instant (unInstant)
import Data.Functor.Tagged (tagged, untagged)
import Data.Int as I
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Number.Format (toStringWith, fixed)
import Data.Time.Duration (Milliseconds(Milliseconds), Minutes, Seconds, convertDuration)
import Data.Traversable (for_)
import Decosim.Sim (Direction(Up, Down, Horizontal), Sim, defaultTimeFactor, initSim, update)
import Decosim.Sim.HaldaneanModel (GasMix(..))
import Decosim.Sim.HaldaneanModel as M
import Decosim.Sim.Types (Bar(Bar))
import Decosim.UI.DiveProfile (drawProfile)
import Decosim.UI.Tissues (GasType(..), drawTissues)
import Graphics.Canvas (CANVAS, CanvasElement)
import Graphics.Canvas as C
import Math ((%))
import Partial.Unsafe (unsafePartial)
import React.DOM as D
import React.DOM.Props as P


sleepTime :: Milliseconds
sleepTime = Milliseconds 50.0

canvas' :: forall e. String -> Eff (canvas :: CANVAS | e) CanvasElement
canvas' id = do
  elm <- C.getCanvasElementById id
  pure $ unsafePartial fromJust elm

panel
  :: forall e props
  . Window
  -> Eff ( state  :: ReactState ReadWrite
         , canvas :: CANVAS
         | e
         ) (ReactClass props)
panel win = do

  let render this = do
        sim <- readState this
        pure $ renderPanel win this sim

      componentDidMount this = do
        addEventListener resize (eventListener \_ -> forceUpdate this) true (windowToEventTarget win)
        forceUpdate this

      componentDidUpdate this _ _ = do
        sim <- readState this
        width  <- innerWidth  win
        height <- innerHeight win

        canvas' "profile-canvas" >>= \canvas -> do
          void $ C.setCanvasDimensions { width: I.toNumber width - 100.0, height: I.toNumber height - 420.0} canvas
          drawProfile canvas sim

        for_ [He, N2] \gasType ->
          canvas' ("tissues-" <> show gasType <> "-canvas") >>= \canvas -> do
            void $ C.setCanvasDimensions { width: (I.toNumber width - 600.0) / 2.0, height: 250.0} canvas
            drawTissues canvas sim gasType

        pure unit

  pure $ createClass $ (spec initSim render)
            { componentDidMount  = componentDidMount
            , componentDidUpdate = componentDidUpdate
            }



run
  :: forall props e
  . ReactThis props Sim
  -> Window
  -> Eff ( console :: CONSOLE
         , dom     :: DOM
         , now     :: NOW
         , ref     :: REF
         , state   :: ReactState ReadWrite
         , timer   :: TIMER
         | e
         ) Unit
run this win = do
    ms0   <- now >>= unInstant >>> pure
    msRef <- R.newRef ms0
    transformState this (_ { paused = false })
    void $ requestAnimationFrame (tick msRef) win
  where
    tick msRef = do
      sim <- readState this
      when (not sim.paused) $ do
        ms <- R.readRef msRef
        nextMs <- now >>= unInstant >>> pure
        let delay = nextMs - ms
        R.writeRef msRef nextMs
        transformState this (update (convertDuration delay))
        forceUpdate this
        void $ setTimeout (I.round $ unwrap sleepTime)
             $ void $ requestAnimationFrame (tick msRef) win



toMin :: Minutes -> String
toMin n =
  let min    = I.round $ unwrap n
      sec    = I.round $ (unwrap $ convertDuration n :: Seconds) % 60.0
      secStr = if sec < 10 then "0" <> show sec
                           else        show sec
  in show min <> ":" <> secStr


renderPanel
  :: forall props
  . Window
  -> ReactThis props Sim
  -> Sim
  -> ReactElement
renderPanel win this sim =
  D.table [P.width "100%"]
          [ D.tbody' [ D.tr' [ D.td [ P.className "title" ] [ D.img [ P.src "img/icon.gif" ] []
                                                            , D.text "Deco Sim"
                                                            ]
                             , D.td [ P.colSpan 2 ] [ ]
                             , D.td [ P.className "github" ] [ D.a [ P.href "https://github.com/colin-passiv/decosim"]
                                                                   [ D.img [ P.src "img/GitHub-Mark-Light-32px.png"] []
                                                                   , D.img [ P.src "img/GitHub_Logo_White.png"
                                                                           , P.height "32", P.alt "GitHub"
                                                                           ] []
                                                                   ]
                                                             ]
                             ]
                     , D.tr' [ D.td [ P.colSpan 4 ] [ D.div [ P.className "alert alert-warning" ] [ D.text "Disclaimer: This is for educational purposes only - not for real dive planning!" ] ] ]
                     , D.tr' [ D.td [ P.style {"verticalAlign": "top"} ]
                                   [ D.table' [ D.tbody' [ D.tr' [ D.th [ P.colSpan 2, P.style {"textAlign": "center"} ] [ D.text "Info" ]]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "Depth (m)"]
                                                                 , D.td [ P.style {"textAlign": "right"}
                                                                        , P.className (if (M.safeAscentDepth sim.model > sim.model.depth) then "danger" else "")
                                                                        ] [ D.text (toStringWith (fixed 1) (unwrap sim.model.depth))]
                                                                 ]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "Dive Time (min)"]
                                                                 , D.td [ P.style {"textAlign": "right"} ] [ D.text (toMin sim.time)]
                                                                 ]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "No Deco Time (min)"]
                                                                 , D.td [ P.style {"textAlign": "right"} ] [ D.text (toMin $ M.noDecoTime sim.model)]
                                                                 ]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "Deco Stop Time (min)"]
                                                                 , D.td [ P.style {"textAlign": "right"}
                                                                        , P.className (if isJust (M.firstDecoStop sim.model) then "warning" else "")
                                                                        ] [ D.text (fromMaybe "" $ map (toMin <<< _.time) $ M.firstDecoStop sim.model) ]
                                                                        -- ] [ D.text (maybe "" $ map (toMin <<< _.time) $ M.firstDecoStop sim.model) ]
                                                                 ]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "Deco Stop Depth (m)"]
                                                                 , D.td [ P.style {"textAlign": "right"}
                                                                        , P.className (if isJust (M.firstDecoStop sim.model) then "warning" else "")
                                                                        ] [ D.text (fromMaybe "" $ map (show <<< unwrap <<< _.depth) $ M.firstDecoStop sim.model) ]
                                                                 ]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "S.A.D. (m)"]
                                                                 , D.td [ P.style {"textAlign": "right"}
                                                                        , P.className (if      (M.safeAscentDepth sim.model <= zero)            then ""
                                                                                       else if (M.safeAscentDepth sim.model >= sim.model.depth) then "danger"
                                                                                       else                                                          "warning"
                                                                                      )
                                                                        ] [ D.text (toStringWith (fixed 1) (unwrap $ M.safeAscentDepth sim.model))]
                                                                  ]
                                                         ]
                                              ]
                                   ]
                            , D.td [ P.style {"verticalAlign": "top"} ]
                                   [ D.table' [ D.tbody' [ D.tr' [ D.th [ P.colSpan 3, P.style {"textAlign": "center"} ] [ D.text "Gas Mix"   ] ]
                                                         , D.tr' [ D.td [ P.colSpan 3 ] [ selectGasMix this sim ] ]
                                                         , D.tr' [ D.th' [ D.text "" ]
                                                                 , D.th' [ D.text "%" ]
                                                                 , D.th' [ D.text "Pa (bar)" ]
                                                                 ]
                                                         , D.tr' [ D.td' [ D.text "Nitrogen" ]
                                                                 , D.td' [ D.text (toStringWith (fixed 2) (untagged $ M.percentN2 sim.model.gasMix)) ]
                                                                 , D.td' [ D.text (toStringWith (fixed 2) (unwrap $ untagged $ M.n2Pa sim.model)) ]
                                                                 ]
                                                         , D.tr' [ D.td' [ D.text "Oxygen" ]
                                                                 , D.td' [ D.text (toStringWith (fixed 2) (untagged $ M.percentO2 sim.model.gasMix)) ]
                                                                 , D.td  [ P.className (if      (M.o2Pa sim.model <= M.o2HypoxiaAta  - tagged (Bar 0.2)) then "warning"
                                                                                        else if (M.o2Pa sim.model >= M.o2ToxicityAta                   ) then "danger"
                                                                                        else if (M.o2Pa sim.model >= M.o2ToxicityAta - tagged (Bar 0.2)) then "warning"
                                                                                        else                                                     ""
                                                                                       )
                                                                         ]
                                                                         [ D.text (toStringWith (fixed 2) (unwrap $ untagged $ M.o2Pa sim.model)) ]
                                                                 ]
                                                         ]
                                              ]
                                   ]
                            , D.td' [ D.table' [ D.tbody' [ D.tr' [ D.th [ P.style {"textAlign": "center"} ] [ D.text "Tissue Compartments Helium" ] ]
                                                          , D.tr' [ D.td' [ D.canvas [P._id "tissues-He-canvas" ] [] ] ]
                                                          ]
                                               ]
                                    ]
                            , D.td' [ D.table' [ D.tbody' [ D.tr' [ D.th [ P.style {"textAlign": "center"} ] [ D.text "Tissue Compartments Nitrogen" ] ]
                                                          , D.tr' [ D.td' [ D.canvas [P._id "tissues-N2-canvas" ] [] ] ]
                                                          ]
                                               ]
                                    ]
                            ]
                      , D.tr' [D.th [ P.colSpan 4, P.style {"textAlign": "center"} ] [D.text "Dive Profile"]]
                      , D.tr' [D.td [ P.colSpan 4 ] [D.table' [D.tbody' [ D.tr' [ D.td [P.style {"verticalAlign": "top"} ]
                                                                                       [D.table' [ D.tbody' (buttons win this sim <#> \btn -> D.tr' [ D.td' [ btn ] ]) ] ]
                                                                                , D.td' [ D.canvas [ P._id "profile-canvas" ] [] ]
                                                                                ]
                                                                        ]
                                                              ]
                                                    ]
                              ]
                    ]
          ]

selectGasMix :: forall props. ReactThis props Sim -> Sim -> ReactElement
selectGasMix this sim =
  D.select' (M.allGasMixes <#> \gasMix ->
    D.option [ P.value (show gasMix)
             , P.default (sim.model.gasMix == gasMix)
             , P.onClick \_ -> modifySim this \sim' -> sim' { model = sim'.model { gasMix = gasMix } } -- TODO create lens
             ]
             [ D.text (show gasMix) ]
   )

buttons :: forall props. Window -> ReactThis props Sim -> Sim -> Array ReactElement
buttons win this sim =
  [ D.button [ P.onClick \_ -> if sim.paused then run this win
                               else               modifySim this $ _ { paused = true }
             ]
             [ if sim.paused then D.img [ P.src "img/start.gif" ] []
               else               D.img [ P.src "img/pause.gif" ] []
             ]
  , D.button [ P.onClick \_ -> modifySim this $ const initSim
             , P.disabled (sim.time == zero)
             ]
             [ D.img [ P.src "img/stop.gif" ] [] ]
  , D.button [ P.onClick \_ -> modifySim this $ _ { direction = Up, timeFactor = defaultTimeFactor }
             , P.disabled (sim.direction == Up)
             ]
             [ D.img [ P.src "img/up.gif" ] [] ]
  , D.button [ P.onClick \_ -> modifySim this $ _ { direction = Horizontal }
             , P.disabled (sim.direction == Horizontal)
             ]
             [ D.img [ P.src "img/horizontal.gif" ] [] ]
  , D.button [ P.onClick \_ -> modifySim this $ _ { direction = Down, timeFactor = defaultTimeFactor }
             , P.disabled (sim.direction == Down)
             ]
             [ D.img [ P.src "img/down.gif" ] [] ]
  , D.button [ P.onClick \_ -> modifySim this $ \sim' -> sim' { timeFactor = max defaultTimeFactor (sim.timeFactor - 1.0) }
             , P.disabled (sim.timeFactor == 1.0)
             ]
             [ D.img [ P.src "img/minus.gif" ] [] ]
  , D.button [ P.onClick \_ -> modifySim this $ \sim' -> sim' { timeFactor = min 15.0 (sim.timeFactor + 1.0) }
             , P.disabled (sim.timeFactor == 10.0)
             ]
             [ D.img [ P.src "img/plus.gif" ] [] ]
  ]

modifySim :: forall props e. ReactThis props Sim -> (Sim -> Sim) -> Eff (state :: ReactState ReadWrite | e) Unit
modifySim this f = do
  transformState this f
  forceUpdate this
