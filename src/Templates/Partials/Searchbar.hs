{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Templates.Partials.Searchbar
Description : Search input with icon button
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

A search bar component with a search icon button and styled text input.
Supports a clear event to reset the input.

<<doc/searchbar.svg>>
-}
module Templates.Partials.Searchbar (
    -- * Search bar

    {- | Search input with icon button. Takes a placeholder text and
    a clear event, returns the input element for reading the value.
    -}
    searchbar,
) where

import Classh
import Data.Default (Default (def))
import Data.Text (Text)
import Reflex.Dom.Core
import Templates.Types (InputEl)

-- | Search input with icon button, styled placeholder, and clear event.
searchbar ::
    (DomBuilder t m) =>
    -- | Placeholder text
    Text ->
    -- | Clear event (resets input to empty)
    Event t a ->
    -- | The input element (read its value via @_inputElement_value@)
    m (InputEl t m)
searchbar placeholder clearEvent = do
    elClass "div" $(classh' [mt .~~ TWSize 0, w .~~ TWSize_Full, bgColor .~~ White, br .~~ R_Normal, custom .~ "flex flex-row"]) $ do
        elClass
            "button"
            $( classh'
                [ px .~~ TWSize 3
                , br .~~ R_Normal
                , custom .~ "leading-none shadow-button focus:outline-none font-icon"
                ]
             )
            $ text "search"
        inputElement $
            def
                & initialAttributes
                .~ ( "class"
                        =: ( $( classh'
                                    [ w .~~ (pix 96)
                                    , h .~~ TWSize_Full
                                    , bgColor .~~ Transparent
                                    , pl .~~ TWSize 2
                                    , py .~~ TWSize 1
                                    , pr .~~ TWSize 3
                                    , custom .~ "focus:outline-none flex-grow placeholder-light"
                                    ]
                              )
                                <> $( classh'
                                        [ text_color .~~ Black
                                        , text_weight .~~ Light
                                        , text_size .~~ XL
                                        , custom .~ "text-icon"
                                        ]
                                    )
                           )
                        <> "placeholder" =: placeholder
                        <> "type" =: "text"
                   )
                & inputElementConfig_setValue
                .~ (mempty <$ clearEvent)
