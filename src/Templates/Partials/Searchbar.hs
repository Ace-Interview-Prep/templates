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
searchbar placeholder clearEvent = do
  elClass "div" $(classh' [mt .~~ TWSize 0, w .~~ TWSize_Full, bgColor .~~ White, br .~~ R_Normal, custom .~ "flex flex-row"]) $ do
    elClass "button" $(classh' [ px .~~ TWSize 3
                               , br .~~ R_Normal
                               , custom .~ "leading-none shadow-button focus:outline-none font-icon"]) $ text "search"
    inputElement $ def
      & initialAttributes .~
        ("class" =: ($(classh' [ w .~~ (pix 96) 
                              , h .~~ TWSize_Full
                              , bgColor .~~ Transparent
                              , pl .~~ TWSize 2
                              , py .~~ TWSize 1
                              , pr .~~ TWSize 3
                              , custom .~ "focus:outline-none flex-grow placeholder-light"
                              ]
                      ) <> $(classh' [ text_color .~~ Black 
                                    , text_weight .~~ Light
                                    , text_size .~~ XL
                                    , custom .~ "text-icon"
                                    ]
                           )
                    )
         <> "placeholder" =: placeholder
         <> "type" =: "text"
        )
      & inputElementConfig_setValue .~ (mempty <$ clearEvent)
  
