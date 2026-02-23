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
toggleButton = toggleButton' (color White)

-- | Parameterized version with custom text color
toggleButton' :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => ColorWithOpacity -> Text -> m (Dynamic t Bool)
toggleButton' txtCol label = do
  let
    classes :: Bool -> Text
    classes shown = classhUnsafe $
      [ custom .~ "text-icon font-icon select-none" ]
      <> (if shown
            then [ transform . rotate .~^ [("def", Rotate_0 `withTransition` Duration_300 `withTiming` Ease_InOut)] ]
            else [ transform . rotate .~^ [("def", Rotate_180 `withTransition` Duration_300 `withTiming` Ease_InOut)] ]
         )

  -- The icon for a rendered container is a triangle pointing up, and
  -- the icon for an unrendered container is that same triangle, rotated
  -- 180deg. Using a single icon + a rotation means the transition can
  -- be animated; Having the "neutral" position be "no transform" means
  -- the icon won't do a twirl on page load.
  rec
    (labelEl, _) <- elClass' "div" (classhUnsafe [ mt .~~ TWSize 8
                                                 , cursor .~~ CursorPointer
                                                 , custom .~ "flex flex-row justify-between"
                                                 ]) $ do
      textS (classhUnsafe [ text_color .~~ txtCol ]) label
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

  toggled <- toggleButtonWithImage imgSrc label
  elDynClass "div" (bodyClasses <$> toggled) body


toggleButtonWithImage :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => Text -> Text -> m (Dynamic t Bool)
toggleButtonWithImage imgSrc label = do
  let
    classes :: Bool -> Text
    classes shown = classhUnsafe $
      [ custom .~ "text-icon font-icon select-none" ]
      <> (if shown
            then [ transform . rotate .~^ [("def", Rotate_0 `withTransition` Duration_300 `withTiming` Ease_InOut)] ]
            else [ transform . rotate .~^ [("def", Rotate_180 `withTransition` Duration_300 `withTiming` Ease_InOut)] ]
         )

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



openCloseButton :: Template t m => ImgSrc -> ColorWithOpacity -> T.Text -> m (Event t ())
openCloseButton imgSrc tColor name = do
  buttonToggleBody (constDyn "pl-10") True $ \case
    True -> do
      --gridCol Col12 $ do
        col [6] $ imgClass imgSrc $(classh' [transform . rotate .~^ [("def", noTransition Rotate_180)], custom .~ "inline-block", position .~~ centered ])
        divClass $(classh' [colSpan .|~ [6], position .~~ centered, custom .~ "inline-block"]) $ do
          textS (classhUnsafe [text_color .~~ tColor]) $ "close" <&> name
    False -> do
      col [6] $ imgClass imgSrc $(classh' [transform . rotate .~^ [("def", noTransition Rotate_180)], custom .~ "inline-block", position .~~ centered ])
      divClass $(classh' [colSpan .|~ [6], position .~~ centered, custom .~ "inline-block"]) $ do
        textS (classhUnsafe [text_color .~~ tColor]) $ "open" <&> name
