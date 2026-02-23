{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Shapes where

import Classh
import Classh.Reflex
import Templates.Types
import Reflex.Dom.Core

type Radius = Float

-- | Circle with specified diameter and background/text colors
-- Example: circle (twSize' 10) (only (noTransition (solidColor aceAccent))) (only (color White))
circle :: DomBuilder t m => TWSizeOrFraction -> WhenTW (WithTransition GradientColor) -> WhenTW ColorWithOpacity -> m ()
circle diameter bgCol txtCol =
  elClass "div" (classhUnsafe [w .~~ diameter, h .~~ diameter, bgColor .~ bgCol, br .~~ R_Full]) $ do
    textS (classhUnsafe [text_color .~ txtCol]) "."

-- | Circle with specified radius and color (for backwards compatibility)
circle' :: DomBuilder t m => Radius -> WhenTW (WithTransition GradientColor) -> WhenTW ColorWithOpacity -> m ()
circle' rad bgCol txtCol = circle (twSize' (rad * 2)) bgCol txtCol

-- | Circle with dynamic color
circleDynColor :: Template t m => TWSizeOrFraction -> Dynamic t GradientColor -> Dynamic t ColorWithOpacity -> m ()
circleDynColor diameter dynBgC dynTxtC = elDynClass "div" (mkBoxStyle <$> dynBgC) $ do
  textDynS (mkTextStyle <$> dynTxtC) "."
  where
    mkTextStyle c' = (classhUnsafe [text_color .~~ c'])
    mkBoxStyle c' = classhUnsafe [ w .~~ diameter
                                 , h .~~ diameter
                                 , bgColor .~~ c'
                                 , br .~~ R_Full
                                 ]
