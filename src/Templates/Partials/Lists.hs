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
data ListItemConfig t = ListItemConfig
  { _listItemConfig_clickable :: Dynamic t Bool
  , _listItemConfig_subtext :: Dynamic t (Maybe Text)
  , _listItemConfig_icon :: Dynamic t (Maybe Text)
  , _listItemConfig_highlight :: Dynamic t (Maybe Text)
  , _listItemConfig_unread :: Dynamic t (Maybe Int)
  }

-- | The default configuration for list items is to have no subtext, no
-- icon, no highlighting information, and to not be clickable.
defListItemConfig :: Applicative (Dynamic t) => ListItemConfig t
defListItemConfig = ListItemConfig
  { _listItemConfig_clickable = pure False
  , _listItemConfig_subtext = pure Nothing
  , _listItemConfig_icon = pure Nothing
  , _listItemConfig_highlight = pure Nothing
  , _listItemConfig_unread = pure Nothing 
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
    topClass :: (Bool, Maybe Int) -> Text
    topClass (click, unread) = T.intercalate " " $
      [ "flex flex-col py-2 border-b border-metaline" ]
      <> case click of
          True -> ["cursor-pointer hover:bg-[#2E3A59]"]
          False -> []
      <> case unread of
           Just _ -> [ "bg-[#FF6D31]" ]
           Nothing -> []
  let topClassDyn = topClass <$> ((,) <$> _listItemConfig_clickable cfg <*> _listItemConfig_unread cfg)

  
  (e, _) <- elDynClass' "div" topClassDyn $ do

    elClass "div" "leading-none font-facit text-body text-xl text-white flex flex-row items-center gap-2 overflow-hidden" $ do
      dyn_ $ ffor (_listItemConfig_icon cfg) $ \case
        Nothing -> blank
        Just icon -> elClass "span" "font-icon text-icon select-none" $ text icon

      el "span" $ dyn_ $ renderHighlight cfg <$> label

    dyn_ $ ffor (_listItemConfig_subtext cfg) $ \case
      Just subtext -> elClass "div" "mt-1 leading-none font-facit text-label text-light" $ text subtext
      Nothing -> blank

  
  pure $ domEvent Click e  



