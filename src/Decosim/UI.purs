module Decosim.UI
  ( panel
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int as I
import Data.Maybe (fromJust, isJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Number.Format (toStringWith, fixed)
import Data.Time.Duration (Milliseconds(Milliseconds), Minutes, Seconds, convertDuration)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Timer (setTimeout)
import Graphics.Canvas (CanvasElement)
import Graphics.Canvas as C
import Math ((%))
import Partial.Unsafe (unsafePartial)
import React as R
import React (ReactClass, ReactThis, ReactElement)
import React.DOM as D
import React.DOM.Props as P
import Unsafe.Coerce (unsafeCoerce)
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.UIEvent.MouseEvent as ME
import Web.UIEvent.MouseEvent.EventTypes (mousemove)
import Web.HTML.HTMLElement (toEventTarget, getBoundingClientRect) as HE
import Web.HTML.Window (Window)
import Web.HTML.Window (innerWidth, innerHeight, requestAnimationFrame, toEventTarget) as W
import Decosim.Sim (Direction(Up, Down, Horizontal), Sim, defaultTimeFactor, initSim, update)
import Decosim.Sim.HaldaneanModel as M
import Decosim.Sim.Types (toNumber)
import Decosim.UI.DiveProfile (drawProfile)
import Decosim.UI.Tissues (GasType(..), drawTissues)


sleepTime ∷ Milliseconds
sleepTime = Milliseconds 50.0

canvas' ∷ String → Effect CanvasElement
canvas' id = do
  elm ← C.getCanvasElementById id
  pure $ unsafePartial fromJust elm

panel ∷ Window → ReactClass {}
panel win =
  R.component "Panel" component
  where
    component this =
      pure { state  : initSim
           , render : render this
           , componentDidMount  : componentDidMount this
           , componentDidUpdate : componentDidUpdate this
           }

    render this = do
        sim ← R.getState this
        pure $ renderPanel win this sim

    componentDidMount this = do
        eventListener (\_ → R.forceUpdate this) >>= \el → addEventListener resize el false (W.toEventTarget win)
        canvas' "profile-canvas" >>= \canvas → do
          let canvasHtmlElement = unsafeCoerce canvas
          eventListener (\e → do
            {top, left} ← HE.getBoundingClientRect canvasHtmlElement
            let me = unsafePartial fromJust $ ME.fromEvent e
            log $  "mouse - client: " <> show (ME.clientX me) <> "," <> show (ME.clientY me)
                <> " canvas: " <> show left <> "," <> show top
                <> "→ " <> show (I.toNumber (ME.clientX me) - left) <> "," <> show (I.toNumber (ME.clientY me) - top)

            ) >>= \el → addEventListener mousemove el false (HE.toEventTarget canvasHtmlElement)
        R.forceUpdate this

    componentDidUpdate this _ _ _ = do
        sim    ← R.getState this
        width  ← W.innerWidth  win
        height ← W.innerHeight win

        canvas' "profile-canvas" >>= \canvas → do
          -- TODO get canvas bounding client position to select the value "420" below...
          void $ C.setCanvasDimensions canvas { width: I.toNumber width - 100.0, height: I.toNumber height - 420.0}
          drawProfile canvas sim

        for_ [He, N2] \gasType →
          canvas' ("tissues-" <> show gasType <> "-canvas") >>= \canvas → do
            void $ C.setCanvasDimensions canvas { width: (I.toNumber width - 600.0) / 2.0, height: 250.0}
            drawTissues canvas sim gasType

        pure unit




run ∷ ∀ props. ReactThis props Sim → Window → Effect Unit
run this win = do
    ms0   ← now >>= unInstant >>> pure
    msRef ← Ref.new ms0
    R.modifyState this _ { paused = false }
    void $ W.requestAnimationFrame (tick msRef) win
  where
    tick msRef = do
      sim ← R.getState this
      when (not sim.paused) do
        ms     ← Ref.read msRef
        nextMs ← now >>= unInstant >>> pure
        let delay = Milliseconds $ unwrap nextMs - unwrap ms
        Ref.write nextMs msRef
        R.modifyState this (update (convertDuration delay))
        void $ setTimeout (I.round $ unwrap sleepTime)
             $ void $ W.requestAnimationFrame (tick msRef) win



toMin ∷ Minutes → String
toMin n =
  let min    = I.round $ unwrap n
      sec    = I.round $ (unwrap $ convertDuration n ∷ Seconds) % 60.0
      secStr = if sec < 10 then "0" <> show sec
                           else        show sec
  in show min <> ":" <> secStr


renderPanel
  ∷ ∀ props
  . Window
  → ReactThis props Sim
  → Sim
  → ReactElement
renderPanel win this sim =
  D.table [P.width "100%"]
          [ D.tbody' [ D.tr' [ D.td [ P.className "title" ] [ D.img [ P.src "img/icon.gif" ]
                                                            , D.text "Deco Sim"
                                                            ]
                             , D.td [ P.colSpan 2 ] [ ]
                             , D.td [ P.className "github" ] [ D.a [ P.href "https://github.com/colin-lamed/decosim"]
                                                                   [ D.img [ P.src "img/GitHub-Mark-Light-32px.png"]
                                                                   , D.img [ P.src "img/GitHub_Logo_White.png"
                                                                           , P.height "32", P.alt "GitHub"
                                                                           ]
                                                                   ]
                                                             ]
                             ]
                     , D.tr' [ D.td [ P.colSpan 4 ] [ D.div [ P.className "alert alert-warning" ] [ D.text "Disclaimer: This is for educational purposes only - not for real dive planning!" ] ] ]
                     , D.tr' [ D.td [ P.style {"verticalAlign": "top"} ]
                                   [ D.table' [ D.tbody' [ D.tr' [ D.th [ P.colSpan 2, P.style {"textAlign": "center"} ] [ D.text "Info" ]]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "Depth (m)"]
                                                                 , D.td [ P.style {"textAlign": "right"}
                                                                        , P.className (if (M.safeAscentDepth sim.model > sim.model.depth) then "danger" else "")
                                                                        ] [ D.text (toStringWith (fixed 1) (toNumber sim.model.depth))]
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
                                                                        ] [ D.text (fromMaybe "" $ map (show <<< toNumber <<< _.depth) $ M.firstDecoStop sim.model) ]
                                                                 ]
                                                         , D.tr' [ D.td [ P.style {"textAlign": "right"} ] [ D.text "S.A.D. (m)"]
                                                                 , D.td [ P.style {"textAlign": "right"}
                                                                        , P.className (if      (M.safeAscentDepth sim.model <= mempty)          then ""
                                                                                       else if (M.safeAscentDepth sim.model >= sim.model.depth) then "danger"
                                                                                       else                                                          "warning"
                                                                                      )
                                                                        ] [ D.text (toStringWith (fixed 1) (toNumber $ M.safeAscentDepth sim.model))]
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
                                                                 , D.td' [ D.text (toStringWith (fixed 2) (toNumber $ M.percentN2 sim.model.gasMix)) ]
                                                                 , D.td' [ D.text (toStringWith (fixed 2) (toNumber $ M.n2Pa sim.model)) ]
                                                                 ]
                                                         , D.tr' [ D.td' [ D.text "Oxygen" ]
                                                                 , D.td' [ D.text (toStringWith (fixed 2) (toNumber $ M.percentO2 sim.model.gasMix)) ]
                                                                 , D.td  [ P.className (if      toNumber (M.o2Pa sim.model) <= toNumber M.o2HypoxiaAta  - 0.2 then "warning"
                                                                                        else if toNumber (M.o2Pa sim.model) >= toNumber M.o2ToxicityAta       then "danger"
                                                                                        else if toNumber (M.o2Pa sim.model) >= toNumber M.o2ToxicityAta - 0.2 then "warning"
                                                                                        else                                                                       ""
                                                                                       )
                                                                         ]
                                                                         [ D.text (toStringWith (fixed 2) (toNumber $ M.o2Pa sim.model)) ]
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
                                                                                       [D.table' [ D.tbody' (buttons win this sim <#> \btn → D.tr' [ D.td' [ btn ] ]) ] ]
                                                                                , D.td' [ D.canvas [ P._id "profile-canvas" ] [] ]
                                                                                ]
                                                                        ]
                                                              ]
                                                    ]
                              ]
                    ]
          ]

selectGasMix ∷ ∀ props. ReactThis props Sim → Sim → ReactElement
selectGasMix this sim =
  D.select' (M.allGasMixes <#> \gasMix →
    D.option [ P.value (show gasMix)
             , P.default (sim.model.gasMix == gasMix)
             , P.onClick \_ → R.modifyState this \sim' → sim' { model = sim'.model { gasMix = gasMix } } -- TODO create lens
             ]
             [ D.text (show gasMix) ]
   )

buttons ∷ ∀ props. Window → ReactThis props Sim → Sim → Array ReactElement
buttons win this sim =
  [ D.button [ P.onClick \_ → if sim.paused then run this win
                               else              R.modifyState this _ { paused = true }
             ]
             [ if sim.paused then D.img [ P.src "img/start.gif" ]
               else               D.img [ P.src "img/pause.gif" ]
             ]
  , D.button [ P.onClick \_ → R.modifyState this $ const initSim
             , P.disabled (sim.time == mempty)
             ]
             [ D.img [ P.src "img/stop.gif" ] ]
  , D.button [ P.onClick \_ → R.modifyState this _ { direction = Up, timeFactor = defaultTimeFactor }
             , P.disabled (sim.direction == Up)
             ]
             [ D.img [ P.src "img/up.gif" ] ]
  , D.button [ P.onClick \_ → R.modifyState this _ { direction = Horizontal }
             , P.disabled (sim.direction == Horizontal)
             ]
             [ D.img [ P.src "img/horizontal.gif" ] ]
  , D.button [ P.onClick \_ → R.modifyState this _ { direction = Down, timeFactor = defaultTimeFactor }
             , P.disabled (sim.direction == Down)
             ]
             [ D.img [ P.src "img/down.gif" ] ]
  , D.button [ P.onClick \_ → R.modifyState this \sim' → sim' { timeFactor = max defaultTimeFactor (sim.timeFactor - 1.0) }
             , P.disabled (sim.timeFactor == 1.0)
             ]
             [ D.img [ P.src "img/minus.gif" ] ]
  , D.button [ P.onClick \_ → R.modifyState this \sim' → sim' { timeFactor = min 15.0 (sim.timeFactor + 1.0) }
             , P.disabled (sim.timeFactor == 10.0)
             ]
             [ D.img [ P.src "img/plus.gif" ] ]
  ]




resize ∷ EventType
resize = EventType "resize"
