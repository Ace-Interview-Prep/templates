{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Main
-- Description : Generate structural SVG diagrams for templates documentation
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com
--
-- Generates structural\/behavioral diagrams for the templates library
-- documentation using the diagrams library. These supplement the
-- hand-crafted component mockups in @doc\/@.
--
-- Run: @cabal run doc-gen -- -o doc\/diagrams.svg -w 800@
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

-- ---------------------------------------------------------------------------
-- renderHighlight diagram
-- ---------------------------------------------------------------------------

-- | Shows how renderHighlight splits text on a token and interleaves
-- bold highlighted segments.
renderHighlightDiagram :: Diagram B
renderHighlightDiagram = vsep 1.5
    [ -- Input
      hsep 0.5 [lbl "Input:", inputBox "Meeting with John"]
      -- Token
    , hsep 0.5 [lbl "Token:", tokenBox "John"]
      -- Split result
    , hsep 0.5
        [ lbl "splitOn:"
        , plainBox "Meeting with "
        , tokenBox "John"
        , plainBox ""
        ]
      -- Rendered output
    , hsep 0.5 [lbl "Rendered:", plainBox "Meeting with ", tokenBox "John"]
    ]

-- | Shows collapsible container state transitions.
collapsibleDiagram :: Diagram B
collapsibleDiagram = hsep 3
    [ vsep 0.5
        [ containerExpanded "Settings" "body content"
        , lbl "toggled = True"
        ]
    , lbl "<-- click -->"
    , vsep 0.5
        [ containerCollapsed "Settings"
        , lbl "toggled = False"
        ]
    ]

-- | Shows modal z-index layering as stacked rectangles.
modalLayerDiagram :: Diagram B
modalLayerDiagram = vsep 0.3
    [ hsep 1 [modalBox, lbl "modal card (z:20, fg)"]
    , hsep 1 [overlayBox, lbl "overlay (z:20, rgba backdrop)"]
    , hsep 1 [pageBox, lbl "page content (z:auto)"]
    ]
  where
    pageBox = roundedRect 8 1.5 0.1 # fc (sRGB24 240 240 240) # lc grey
    overlayBox = roundedRect 8 1.5 0.1 # fc (sRGB24 180 180 180) # lc grey
    modalBox = roundedRect 6 1.5 0.1 # fc (sRGB24 0 0 77) # lc black

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

lbl :: String -> Diagram B
lbl s = textt s # fontSizeL 0.5 # fc black

textt :: String -> Diagram B
textt s = text s <> strutX (fromIntegral (length s) * 0.3) <> strutY 0.6

inputBox :: String -> Diagram B
inputBox s =
    text s # fontSizeL 0.45 # fc (sRGB24 50 50 50)
        <> roundedRect 6 0.8 0.1 # fc (sRGB24 245 245 245) # lc (sRGB24 200 200 200)

plainBox :: String -> Diagram B
plainBox s =
    text s # fontSizeL 0.4 # fc (sRGB24 80 80 80)
        <> roundedRect (max 1.5 (fromIntegral (length s) * 0.28)) 0.7 0.1 # fc white # lc (sRGB24 200 200 200)

tokenBox :: String -> Diagram B
tokenBox s =
    text s # fontSizeL 0.4 # fc white
        <> roundedRect (max 1.5 (fromIntegral (length s) * 0.32)) 0.7 0.1 # fc (sRGB24 0 185 218) # lc (sRGB24 0 185 218)

containerExpanded :: String -> String -> Diagram B
containerExpanded title body = vsep 0
    [ text title # fontSizeL 0.45 # fc white
        <> roundedRect 6 1 0.1 # fc (sRGB24 37 37 64)
    , text body # fontSizeL 0.4 # fc (sRGB24 150 150 150)
        <> rect 6 3 # fc (sRGB24 26 26 46) # lc (sRGB24 51 51 51)
    ]

containerCollapsed :: String -> Diagram B
containerCollapsed title =
    text title # fontSizeL 0.45 # fc white
        <> roundedRect 6 1 0.1 # fc (sRGB24 37 37 64) # lc (sRGB24 51 51 51)

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = mainWith $ vsep 4
    [ lbl "renderHighlight" === strutY 0.5 === renderHighlightDiagram
    , lbl "Collapsible Container" === strutY 0.5 === collapsibleDiagram
    , lbl "Modal Layers" === strutY 0.5 === modalLayerDiagram
    ]
