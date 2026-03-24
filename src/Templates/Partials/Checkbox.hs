{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : Templates.Partials.Checkbox
Description : Custom checkbox component with cleaner API
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Wraps reflex-dom's 'InputElement' into a simpler 'Checkbox' type
with a 'HasValue' instance providing @Dynamic t Bool@.

<<doc/checkbox.svg>>
-}
module Templates.Partials.Checkbox (
    -- * Configuration

    {- | Configuration for a checkbox element. Set an external event to
    change the checked state, and\/or provide dynamic attributes.
    -}
    CheckboxConfig (..),

    -- * Checkbox type

    {- | Result of rendering a checkbox. Provides the current checked state
    as a 'Dynamic' and change events.
    -}
    Checkbox (..),

    -- * Rendering

    -- | Render a checkbox element with the given initial state and config.
    checkbox,
) where

import Data.Default (Default (def))
import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom.Core

-- | Configuration for a checkbox element.
data CheckboxConfig t = CheckboxConfig
    { _checkboxConfig_setValue :: Event t Bool
    -- ^ External event to set the checked state.
    , _checkboxConfig_attributes :: Dynamic t (Map.Map T.Text T.Text)
    -- ^ Dynamic attributes for the checkbox element.
    }

instance (Reflex t) => Default (CheckboxConfig t) where
    {-# INLINEABLE def #-}
    def =
        CheckboxConfig
            { _checkboxConfig_setValue = never
            , _checkboxConfig_attributes = constDyn mempty
            }

-- | Result of rendering a checkbox.
data Checkbox t = Checkbox
    { _checkbox_value :: Dynamic t Bool
    -- ^ Current checked state as a 'Dynamic'.
    , _checkbox_change :: Event t Bool
    -- ^ Event that fires when the checked state changes.
    }

instance HasValue (Checkbox t) where
    type Value (Checkbox t) = Dynamic t Bool
    value = _checkbox_value

-- | Render a checkbox element with the given initial state and config.
checkbox :: (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox checked config = do
    let permanentAttrs = "type" =: "checkbox"
        dAttrs = Map.delete "checked" . Map.union permanentAttrs <$> _checkboxConfig_attributes config
    modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
    i <-
        inputElement $
            def
                & inputElementConfig_initialChecked
                .~ checked
                & inputElementConfig_setChecked
                .~ _checkboxConfig_setValue config
                & inputElementConfig_elementConfig
                . elementConfig_initialAttributes
                .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
                & inputElementConfig_elementConfig
                . elementConfig_modifyAttributes
                .~ fmap mapKeysToAttributeName modifyAttrs
    return $
        Checkbox
            { _checkbox_value = _inputElement_checked i
            , _checkbox_change = _inputElement_checkedChange i
            }
