{-# LANGUAGE OverloadedStrings #-}

-- | Partial for rendering list items.
module Templates.Partials.Lists where

import Classh
import Classh.Reflex (textS)

import Data.Default
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core

-- | Configuration for a list item.
-- All color fields use WhenTW for full user control over states and transitions.
data ListItemConfig t = ListItemConfig
  { _listItemConfig_clickable :: Dynamic t Bool
  , _listItemConfig_subtext :: Dynamic t (Maybe Text)
  , _listItemConfig_icon :: Dynamic t (Maybe Text)
  , _listItemConfig_highlight :: Dynamic t (Maybe Text)
  , _listItemConfig_unread :: Dynamic t (Maybe Int)
  , _listItemConfig_bgColor :: WhenTW (WithTransition GradientColor)
  , _listItemConfig_borderColor :: WhenTW (WithTransition Color)
  , _listItemConfig_textColor :: WhenTW Color
  , _listItemConfig_subtextColor :: WhenTW Color
  }

-- | The default configuration for list items is to have no subtext, no
-- icon, no highlighting information, and to not be clickable.
-- Uses default dark theme colors.
defListItemConfig :: Applicative (Dynamic t) => ListItemConfig t
defListItemConfig = defListItemConfig'
  (only (noTransition (solidColor Transparent)))
  (only (noTransition (hex "E5E7EB")))
  (only White)
  (only (hex "D1D5DB"))

-- | Configurable version with custom colors
defListItemConfig'
  :: Applicative (Dynamic t)
  => WhenTW (WithTransition GradientColor) -- ^ Background color (user controls states/transitions)
  -> WhenTW (WithTransition Color) -- ^ Border color
  -> WhenTW Color -- ^ Text color
  -> WhenTW Color -- ^ Subtext color
  -> ListItemConfig t
defListItemConfig' bgCol borderCol textCol subtextCol = ListItemConfig
  { _listItemConfig_clickable = pure False
  , _listItemConfig_subtext = pure Nothing
  , _listItemConfig_icon = pure Nothing
  , _listItemConfig_highlight = pure Nothing
  , _listItemConfig_unread = pure Nothing
  , _listItemConfig_bgColor = bgCol
  , _listItemConfig_borderColor = borderCol
  , _listItemConfig_textColor = textCol
  , _listItemConfig_subtextColor = subtextCol
  }

instance Reflex t => Default (ListItemConfig t) where
  def = defListItemConfig

-- | Render the given 'Text' with highlighting given by the
-- 'ListItemConfig'.
renderHighlight :: (DomBuilder t m, PostBuild t m) => ListItemConfig t -> Text -> m ()
renderHighlight cfg txt = dyn_ $ ffor (_listItemConfig_highlight cfg) $ \case
  Just tok | not (T.null tok) -> 
    sequence_ $ intersperse 
      (elClass "span" "font-bold" (text tok))  -- Bold occurences of the highlighted text,
      (text <$> T.splitOn tok txt)             -- inbetween the rest of the label text
  _ -> text txt -- If there is no highlighting information, just render it as text.


-- | Render a list item with the given label.
listItem
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => ListItemConfig t -- ^ Visual configuration for the list item
  -> Dynamic t Text -- ^ The label
  -> m (Event t ())
  -- ^ If the configuration has '_listItemConfig_clickable' set, then
  -- this is the 'Click' event for the list item. Otherwise, it's
  -- 'never'.
listItem cfg label = do
  let
    topClass :: Bool -> Text
    topClass click = classhUnsafe $
      [ py .~~ TWSize 2
      , border . bStyle .~~ BSolid
      , bw_b .~~ B1
      , bc_b .~ _listItemConfig_borderColor cfg
      , bgColor .~ _listItemConfig_bgColor cfg
      ]
      <> if click then [ cursor .~~ CursorPointer ] else []
  let topClassDyn = topClass <$> _listItemConfig_clickable cfg

  (e, _) <- elDynClass' "div" topClassDyn $ do

    let labelClass = classhUnsafe [ custom .~ "leading-none font-facit text-body text-xl flex flex-row items-center gap-2 overflow-hidden"
                                  , text_color .~ _listItemConfig_textColor cfg
                                  ]
    elClass "div" labelClass $ do
      dyn_ $ ffor (_listItemConfig_icon cfg) $ \case
        Nothing -> blank
        Just icon -> elClass "span" "font-icon text-icon select-none" $ text icon

      el "span" $ dyn_ $ renderHighlight cfg <$> label

    dyn_ $ ffor (_listItemConfig_subtext cfg) $ \case
      Just subtext -> textS (classhUnsafe [ custom .~ "mt-1 leading-none font-facit text-label"
                                          , text_color .~ _listItemConfig_subtextColor cfg
                                          ]) subtext
      Nothing -> blank


  pure $ domEvent Click e  



