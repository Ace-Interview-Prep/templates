{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Shapes where

import Classh
import Classh.Reflex
import Templates.Types
import Reflex.Dom.Core

type Radius = Float

-- | Circle with specified diameter and color
-- Example: circle (twSize' 10) aceAccent creates a 10-unit diameter circle
circle :: DomBuilder t m => TWSizeOrFraction -> Color -> m ()
circle diameter c =
  elClass "div" (classhUnsafe [w .~~ diameter, h .~~ diameter, bgColor .~~ c, br .~~ R_Full]) $ do
    textS (classhUnsafe [text_color .~~ c]) "."

-- | Circle with specified radius and color (for backwards compatibility)
circle' :: DomBuilder t m => Radius -> Color -> m ()
circle' rad c = circle (twSize' (rad * 2)) c

-- | Circle with dynamic color
circleDynColor :: Template t m => TWSizeOrFraction -> Dynamic t Color -> m ()
circleDynColor diameter dynC = elDynClass "div" (mkBoxStyle <$> dynC) $ do
  textDynS (mkTextStyle <$> dynC) "."
  where
    mkTextStyle c' = (classhUnsafe [text_color .~~ c'])
    mkBoxStyle c' = classhUnsafe [ w .~~ diameter
                                 , h .~~ diameter
                                 , bgColor .~~ c'
                                 , br .~~ R_Full
                                 ]
