{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Containers.Dropdown where

import Classh
import Classh.Reflex
import Reflex.Dom.Core
import Control.Monad.Fix
import Data.Maybe
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Text as T


-- | TODO: take an element config for the options elements as well
-- | TODO: clean up and upstream to reflex-dom-contrib
dropdown'
  :: ( MonadFix m
     , DomBuilder t m
     )
  => Map.Map a T.Text
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Dynamic t a)
dropdown' = dropdown'' (hex "00B9DA") White (Gray C300)

-- | Parameterized version with custom colors
dropdown''
  :: ( MonadFix m
     , DomBuilder t m
     )
  => Color -- ^ Focus border color
  -> Color -- ^ Background color
  -> Color -- ^ Default border color
  -> Map.Map a T.Text
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Dynamic t a)
dropdown'' focusCol bgCol borderCol options cfg' = mdo
  let class' = classhUnsafe [ w .~~ TWSize_Full
                            , px .~~ TWSize 4
                            , py .~~ TWSize 3
                            , bw .~~ B1
                            , bc .~^ [("def", noTransition borderCol), ("focus", noTransition focusCol)]
                            , br .~~ R_Lg
                            , border . outline .~ [("focus", Outline_None)]
                            , mb .~~ TWSize 5
                            , bgColor .~~ bgCol
                            , custom .~ "font-[Sarabun] text-lg"
                            ]
  let safeInitial = case Map.toList options of
        [] -> error "dropdown' requires non-empty options map"
        (k,v):_ -> v
  let cfg = cfg'
            & selectElementConfig_initialValue .~ safeInitial
            & selectElementConfig_setValue .~ optionsEvent
            & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: class')
  (selectEl, optionsEvent) <- selectElement cfg $ do
    optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
    pure $ leftmost optionsEv
  let options' = Map.fromList $ fmap flipTup $ Map.toList options
  pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
  where
    flipTup (a_,b_) = (b_,a_)
    makeOpt optText = do
      (e, _) <- elAttr' "option" ("value" =: optText) $ text optText
      pure $ optText <$ domEvent Click e

dropdownWithDefault
  :: ( MonadFix m
     , DomBuilder t m
     )
  => Map.Map a T.Text
  -> T.Text
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Dynamic t a)
dropdownWithDefault = dropdownWithDefault' (hex "00B9DA") White (Gray C300)

-- | Parameterized version with custom colors
dropdownWithDefault'
  :: ( MonadFix m
     , DomBuilder t m
     )
  => Color -- ^ Focus border color
  -> Color -- ^ Background color
  -> Color -- ^ Default border color
  -> Map.Map a T.Text
  -> T.Text
  -> SelectElementConfig er t (DomBuilderSpace m)
  -> m (Dynamic t a)
dropdownWithDefault' focusCol bgCol borderCol options start cfg' = mdo
  let class' = classhUnsafe [ w .~~ TWSize_Full
                            , px .~~ TWSize 4
                            , py .~~ TWSize 3
                            , bw .~~ B1
                            , bc .~^ [("def", noTransition borderCol), ("focus", noTransition focusCol)]
                            , br .~~ R_Lg
                            , border . outline .~ [("focus", Outline_None)]
                            , mb .~~ TWSize 5
                            , bgColor .~~ bgCol
                            ]
  let safeInitial = start
  let cfg = cfg'
            & selectElementConfig_initialValue .~ safeInitial
            & selectElementConfig_setValue .~ optionsEvent
            & selectElementConfig_elementConfig . elementConfig_initialAttributes .~ ("class" =: class')
  (selectEl, optionsEvent) <- selectElement cfg $ do
    optionsEv <- mapM makeOpt $ fmap snd $ Map.toList options
    pure $ leftmost optionsEv
  let options' = Map.fromList $ fmap flipTup $ Map.toList options
  pure $ fmap (\v -> fromJust $ Map.lookup v options') $ _selectElement_value selectEl
  where
    flipTup (a_,b_) = (b_,a_)
    makeOpt optText = do
      (e, _) <- elAttr' "option" ("value" =: optText) $ textS (classhUnsafe [text_font .~~ Font_Custom "Sarabun", text_size .~~ LG]) optText
      pure $ optText <$ domEvent Click e
