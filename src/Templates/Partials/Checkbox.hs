{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Checkbox where

import Reflex.Dom.Core hiding (checkbox, Checkbox(..), CheckboxConfig(..))
import Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T



data CheckboxConfig t = CheckboxConfig
  { _checkboxConfig_setValue :: Event t Bool
  , _checkboxConfig_attributes :: Dynamic t (Map.Map T.Text T.Text)
  }

instance Reflex t => Default (CheckboxConfig t) where
  {-# INLINABLE def #-}
  def = CheckboxConfig { _checkboxConfig_setValue = never
                       , _checkboxConfig_attributes = constDyn mempty
                       }

data Checkbox t
   = Checkbox { _checkbox_value :: Dynamic t Bool
              , _checkbox_change :: Event t Bool
              }

instance HasValue (Checkbox t) where
  type Value (Checkbox t) = Dynamic t Bool
  value = _checkbox_value

checkbox :: (DomBuilder t m, PostBuild t m) => Bool -> CheckboxConfig t -> m (Checkbox t)
checkbox checked config = do
  let permanentAttrs = "type" =: "checkbox"
      dAttrs = Map.delete "checked" . Map.union permanentAttrs <$> _checkboxConfig_attributes config
  modifyAttrs <- dynamicAttributesToModifyAttributes dAttrs
  i <- inputElement $ def
    & inputElementConfig_initialChecked .~ checked
    & inputElementConfig_setChecked .~ _checkboxConfig_setValue config
    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ Map.mapKeys (AttributeName Nothing) permanentAttrs
    & inputElementConfig_elementConfig . elementConfig_modifyAttributes .~ fmap mapKeysToAttributeName modifyAttrs
  return $ Checkbox
    { _checkbox_value = _inputElement_checked i
    , _checkbox_change = _inputElement_checkedChange i
    }
