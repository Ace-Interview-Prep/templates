{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Templates.Partials.Searchbar where

import Classh

import Data.Default
import Data.Text (Text)

import Reflex.Dom.Core
import Templates.Types

searchbar
  :: DomBuilder t m
  => Text
  -> Event t a
  -> m (InputEl t m)
searchbar = searchbar'
  (only (noTransition (solidColor White)))
  (only (noTransition (solidColor Transparent)))
  (only Black)

-- | Parameterized version with custom colors
searchbar'
  :: DomBuilder t m
  => WhenTW (WithTransition GradientColor) -- ^ Container background color
  -> WhenTW (WithTransition GradientColor) -- ^ Input background color
  -> WhenTW Color -- ^ Text color
  -> Text
  -> Event t a
  -> m (InputEl t m)
searchbar' bgCol inputBgCol txtCol placeholder clearEvent = do
  elClass "div" (classhUnsafe [mt .~~ TWSize 0, w .~~ TWSize_Full, bgColor .~ bgCol, br .~~ R_Normal, custom .~ "flex flex-row"]) $ do
    elClass "button" (classhUnsafe [ px .~~ TWSize 3
                                   , br .~~ R_Normal
                                   , shadow .~~ Shadow_Md
                                   , border . outline .~ [("focus", Outline_None)]
                                   , custom .~ "leading-none font-icon"
                                   ]) $ text "search"
    let inputClass = classhUnsafe [ w .~~ (pix 96)
                                  , h .~~ TWSize_Full
                                  , bgColor .~ inputBgCol
                                  , pl .~~ TWSize 2
                                  , py .~~ TWSize 1
                                  , pr .~~ TWSize 3
                                  , border . outline .~ [("focus", Outline_None)]
                                  , custom .~ "flex-grow placeholder-light"
                                  ]
                     <> classhUnsafe [ text_color .~ txtCol
                                     , text_weight .~~ Light
                                     , text_size .~~ XL
                                     , custom .~ "text-icon"
                                     ]
    inputElement $ def
      & initialAttributes .~ ("class" =: inputClass
                              <> "placeholder" =: placeholder
                              <> "type" =: "text")
      & inputElementConfig_setValue .~ (mempty <$ clearEvent)
  
