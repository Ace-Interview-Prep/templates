{-# LANGUAGE OverloadedStrings #-}
module Templates.Partials.Containers where

import Classh
import Classh.Reflex
import Templates.Partials.Image
import Templates.Partials.Buttons
import Templates.Types

import Data.Text as T (Text)
import Data.Bool
import Control.Monad.Fix
import Control.Monad
import Reflex.Dom.Core

screenContainer :: (DomBuilder t m) => m a -> m a
screenContainer = elClass "div" $(classh' [w .~~ TWSize_Screen, h .~~ TWSize_Screen, custom .~ "flex flex-col overflow-hidden"])

toggleButton :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Text -> m (Dynamic t Bool)
toggleButton label = do
  let
    classes :: Bool -> Text
    classes shown =
      "text-icon font-icon transform select-none "           -- Icon display classes
        <> "transition-transform duration-300 ease-in-out "  -- Animation settings classes
        <> bool "rotate-180" mempty shown                    -- Do a little twirl!

  -- The icon for a rendered container is a triangle pointing up, and
  -- the icon for an unrendered container is that same triangle, rotated
  -- 180deg. Using a single icon + a rotation means the transition can
  -- be animated; Having the "neutral" position be "no transform" means
  -- the icon won't do a twirl on page load.
  rec
    (labelEl, _) <- elClass' "div" $(classh' [ mt .~~ TWSize 8
                                             , custom .~ "cursor-pointer  flex flex-row justify-between"
                                             ]) $ do
      textS $(classh' [ text_color .~~ White ]) label
      elDynClass' "span" (classes <$> toggled) $ text "expand_less"
    let toggleEv = domEvent Click labelEl
    toggled <- holdUniqDyn =<< toggle True toggleEv

  pure toggled




-- | Wrap the body contents in a container that can be collapsed by
-- clicking on its header. As an optimisation, when the container is
-- collapsed, the inner tree is not rendered.
collapsibleContainer_ :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Text -> m a -> m ()
collapsibleContainer_ label body = do
  toggled <- toggleButton label

  dyn_ (bool blank (void body) <$> toggled)

-- | Wrap the body contents in a container that can be collapsed by
-- clicking on its header.
collapsibleContainer :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Text -> m a -> m a
collapsibleContainer label body = do
  let
    bodyClasses :: Bool -> Text
    bodyClasses shown = bool "hidden" mempty shown

  toggled <- toggleButton label

  elDynClass "div" (bodyClasses <$> toggled) body


-- | Wrap the body contents in a container that can be collapsed by
-- clicking on its header.
collapsibleContainerWithImage
  :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m)
  => Text
  -> Text
  -> m a
  -> m a
collapsibleContainerWithImage imgSrc label body = do
  let
    bodyClasses :: Bool -> Text
    bodyClasses shown = bool "hidden" mempty shown

  toggled <- toggleButton' imgSrc label
  elDynClass "div" (bodyClasses <$> toggled) body


toggleButton' :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Text -> Text -> m (Dynamic t Bool)
toggleButton' imgSrc label = do
  let
    classes :: Bool -> Text
    classes shown =
      "text-icon font-icon transform select-none "           -- Icon display classes
        <> "transition-transform duration-300 ease-in-out "  -- Animation settings classes
        <> bool "rotate-180" mempty shown                    -- Do a little twirl!

  -- The icon for a rendered container is a triangle pointing up, and
  -- the icon for an unrendered container is that same triangle, rotated
  -- 180deg. Using a single icon + a rotation means the transition can
  -- be animated; Having the "neutral" position be "no transform" means
  -- the icon won't do a twirl on page load.
  rec
    (labelEl, _) <- elClass' "div" "grid grid-cols-12 p-4" $ do
      elClass "div" $(classh' [colSpan .~~ 2]) $ imgClass imgSrc ""
      elClass "div" $(classh' [colSpan .~~ 9]) $ textS $(classh' [text_size .|~ [Base,LG,XL,XL2, XL3]]) label
      elDynClass' "span" (classes <$> toggled) $
        dynText "expand_less"

    let toggleEv = domEvent Click labelEl
    toggled <- holdUniqDyn =<< toggle True toggleEv

  pure toggled



openCloseButton :: Template t m => ImgSrc -> Color -> T.Text -> m (Event t ())
openCloseButton imgSrc tColor name = do
  buttonToggleBody (constDyn "pl-10") True $ \case
    True -> do
      --gridCol Col12 $ do
        col [6] $ imgClass imgSrc $(classh' [custom .~ "rotate-180 inline-block", position .~~ centered ])
        divClass $(classh' [colSpan .|~ [6], position .~~ centered, custom .~ "inline-block"]) $ do
          textS (classhUnsafe [text_color .~~ tColor]) $ "close" <&> name
    False -> do
      col [6] $ imgClass imgSrc $(classh' [custom .~ "rotate-180 inline-block", position .~~ centered ])
      divClass $(classh' [colSpan .|~ [6], position .~~ centered, custom .~ "inline-block"]) $ do
        textS (classhUnsafe [text_color .~~ tColor]) $ "open" <&> name
