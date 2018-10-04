module Main where

import Prelude
import Data.Maybe (fromJust)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import React as R
import ReactDOM (render)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

import Decosim.UI (panel)

main ∷ Effect Unit
main = do
    win    ← window
    doc    ← document win
    let parentNode = toNonElementParentNode doc
    elm    ← elm' parentNode
    void $ render (R.createLeafElement (panel win) {}) elm
  where
    elm' parentNode = do
      elm ← getElementById "main" parentNode
      pure $ unsafePartial fromJust elm
