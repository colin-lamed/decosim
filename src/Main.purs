module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Maybe (fromJust)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (Window, htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), NonElementParentNode, documentToNonElementParentNode)
import Partial.Unsafe (unsafePartial)
import React (ReactElement, ReactState, ReadWrite, createFactory)
import React.DOM as D
import ReactDOM (render)
import Graphics.Canvas (CANVAS)

import Decosim.UI (panel)

type E = ( canvas  :: CANVAS
         , console :: CONSOLE
         , dom     :: DOM
         , state   :: ReactState ReadWrite
         )

main :: Eff E Unit
main = do
    win    <- window
    doc    <- document win
    let parentNode = documentToNonElementParentNode (htmlDocumentToDocument doc)
    elm    <- elm' parentNode
    ui'    <- ui win
    void $ render ui' elm
  where
    ui :: Window -> Eff E ReactElement
    ui win = do
         ip <- panel win
         pure $ D.div [] [ createFactory ip unit]
    elm' :: NonElementParentNode -> Eff E Element
    elm' parentNode = do
      elm <- getElementById (ElementId "main") parentNode
      pure $ unsafePartial fromJust elm
