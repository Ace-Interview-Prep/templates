{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Shapes where

import Classh
import Classh.Reflex
import Templates.Types
import Reflex.Dom.Core
import qualified Data.Text as T

-- | Simple circle with specified radius and color
circle :: DomBuilder t m => T.Text -> Color -> m ()
circle rad c = circle' (parseRadius rad) c
  where
    -- Parse "10px" to 5.0 (radius is half diameter)
    parseRadius r = case T.stripSuffix "px" r of
      Just numStr -> case reads (T.unpack numStr) :: [(Float, String)] of
        [(n, "")] -> n / 2
        _ -> 5.0 -- default
      Nothing -> 5.0

-- | Circle with specified radius and color using ClasshSS
circle' :: DomBuilder t m => Radius -> Color -> m ()
circle' rad c = elClass "div" (classhUnsafe [w .~~ twSize' (rad * 2), h .~~ twSize' (rad * 2), bgColor .~~ c, br .~~ R_Full]) $ do
  textS (classhUnsafe [text_color .~~ c]) "."

-- | Circle with dynamic color
circleDynColor' :: Template t m => Radius -> Dynamic t Color -> m ()
circleDynColor' rad dynC = elDynClass "div" (mkBoxStyle <$> dynC) $ do
  textDynS (mkTextStyle <$> dynC) "."
  where
    mkTextStyle c' = (classhUnsafe [text_color .~~ c'])
    mkBoxStyle c' = classhUnsafe [ w .~~ twSize' (rad * 2)
                                 , h .~~ twSize' (rad * 2)
                                 , bgColor .~~ c'
                                 , br .~~ R_Full
                                 ]

-- | Legacy dynamic color circle (uses raw HTML)
circleDynColor ::  (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t T.Text -> m ()
circleDynColor rad dynColor =
  elDynAttr "span" attrs blank
  where
    attrs = ffor dynColor $ \color ->
      ( "style" =: ("border-radius:50%; width: " <> rad <> "; height: " <> rad <> "; display: inline-block;")
        <> "class" =: color
      )

type Radius = Float
