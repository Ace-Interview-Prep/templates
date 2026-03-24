{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Partials.Containers.Dropdown
Description : Styled dropdown select components
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Dropdown select elements with ClasshSS styling. Provides both a basic
dropdown and one with a configurable default selection.

<<doc/dropdown.svg>>
-}
module Templates.Partials.Containers.Dropdown (
    -- * Dropdowns

    {- | Styled dropdown select. Takes a map of options and returns a
    'Dynamic' tracking the currently selected key.

    __Warning:__ Uses 'Data.Maybe.fromJust' internally — will crash
    if the selected value is not found in the options map.
    -}
    dropdown',
    -- | Dropdown with a configurable initial text value.
    dropdownWithDefault,
) where

import Classh
import Classh.Reflex (textS)
import Control.Monad.Fix (MonadFix)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Reflex.Dom.Core

-- | Styled dropdown select element.
dropdown' ::
    (MonadFix m, DomBuilder t m) =>
    Map.Map a T.Text ->
    SelectElementConfig er t (DomBuilderSpace m) ->
    m (Dynamic t a)
dropdown' options cfg' = mdo
    let class' = "w-full px-4 py-3 border border-gray-300 rounded-lg focus:outline-none focus:border-[#00B9DA] font-[Sarabun] text-lg mb-5 bg-white"
    let safeInitial = (snd . head $ Map.toList options)
    let cfg =
            cfg'
                & selectElementConfig_initialValue
                .~ safeInitial
                & selectElementConfig_setValue
                .~ optionsEvent
                & selectElementConfig_elementConfig
                . elementConfig_initialAttributes
                .~ ("class" =: class')
    (selectEl, optionsEvent) <- selectElement cfg $ do
        optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
        pure $ leftmost optionsEv
    let options' = Map.fromList $ fmap flipTup $ Map.toList options
    pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
  where
    flipTup (a_, b_) = (b_, a_)
    makeOpt optText = do
        (e, _) <- elAttr' "option" ("value" =: optText) $ text optText
        pure $ optText <$ domEvent Click e

-- | Dropdown with a configurable initial text value and ClasshSS styling.
dropdownWithDefault ::
    (MonadFix m, DomBuilder t m) =>
    Map.Map a T.Text ->
    T.Text ->
    SelectElementConfig er t (DomBuilderSpace m) ->
    m (Dynamic t a)
dropdownWithDefault options start cfg' = mdo
    let class' =
            $( classh'
                [ w .~~ TWSize_Full
                , px .~~ TWSize 4
                , py .~~ TWSize 3
                , bw .~~ B1
                , bc .~^ [("def", noTransition (Gray C300)), ("focus", noTransition (hex "00B9DA"))]
                , br .~~ R_Lg
                , border . bStyle .~ [("focus", BNone)]
                , custom .~ "focus:outline-none focus:border-"
                , mb .~~ TWSize 5
                , bgColor .~~ White
                ]
             )
    let safeInitial = start
    let cfg =
            cfg'
                & selectElementConfig_initialValue
                .~ safeInitial
                & selectElementConfig_setValue
                .~ optionsEvent
                & selectElementConfig_elementConfig
                . elementConfig_initialAttributes
                .~ ("class" =: class')
    (selectEl, optionsEvent) <- selectElement cfg $ do
        optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
        pure $ leftmost optionsEv
    let options' = Map.fromList $ fmap flipTup $ Map.toList options
    pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
  where
    flipTup (a_, b_) = (b_, a_)
    makeOpt optText = do
        (e, _) <- elAttr' "option" ("value" =: optText) $ textS $(classh' [text_font .~~ Font_Custom "Sarabun", text_size .~~ LG]) optText
        pure $ optText <$ domEvent Click e
