{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Partials.Containers
Description : Collapsible containers with animated toggle arrows
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Container components that can be collapsed by clicking their header.
The toggle arrow animates with a CSS rotation transition.

<<doc/containers.svg>>
-}
module Templates.Partials.Containers (
    -- * Screen container

    -- | Full viewport-sized container.
    screenContainer,

    -- * Toggle buttons

    {- | Render a toggle button that returns a 'Dynamic' tracking the
    expanded\/collapsed state.
    -}
    toggleButton,
    -- | Toggle button variant with an image icon.
    toggleButton',

    -- * Collapsible containers

    {- | Collapsible container that hides\/shows the body via CSS (body
    remains in the DOM).
    -}
    collapsibleContainer,
    {- | Collapsible container that does not render the body when collapsed
    (optimisation: inner tree is destroyed).
    -}
    collapsibleContainer_,
    -- | Collapsible container with an image icon in the header.
    collapsibleContainerWithImage,

    -- * Open\/close button

    -- | Toggle button with an image that shows \"open\"\/\"close\" text.
    openCloseButton,
) where

import Classh as C
import Classh.Reflex as CR
import Control.Monad (void)
import Data.Bool (bool)
import qualified Data.Text as T
import Reflex.Dom.Core
import Templates.Partials.Buttons (buttonToggleBody)
import Templates.Partials.Image (imgClass)
import Templates.Types (ImgSrc, Template)

-- | Full viewport-sized container.
screenContainer :: (DomBuilder t m) => m a -> m a
screenContainer = elClass "div" $(classh' [w .~~ TWSize_Screen, h .~~ TWSize_Screen, custom .~ "flex flex-col overflow-hidden"])

{- | Render a toggle button with animated arrow. Returns 'Dynamic' tracking
the expanded\/collapsed state (starts expanded).
-}
toggleButton :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => T.Text -> m (Dynamic t Bool)
toggleButton label = do
    let
        classes :: Bool -> T.Text
        classes shown =
            "text-icon font-icon transform select-none "
                <> "transition-transform duration-300 ease-in-out "
                <> bool "rotate-180" mempty shown

    rec (labelEl, _) <-
            elClass'
                "div"
                $(classh' [mt .~~ TWSize 8, custom .~ "cursor-pointer  flex flex-row justify-between"])
                $ do
                    CR.textS $(classh' [text_color .~~ White]) label
                    elDynClass' "span" (classes <$> toggled) $ text "expand_less"
        let toggleEv = domEvent Click labelEl
        toggled <- holdUniqDyn =<< toggle True toggleEv

    pure toggled

{- | Collapsible container that does not render the body when collapsed.
As an optimisation, the inner tree is destroyed when collapsed.
-}
collapsibleContainer_ :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => T.Text -> m a -> m ()
collapsibleContainer_ label body = do
    toggled <- toggleButton label

    dyn_ (bool blank (void body) <$> toggled)

{- | Collapsible container that hides\/shows the body via CSS.
The body remains in the DOM when collapsed (uses @hidden@ class).
-}
collapsibleContainer :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => T.Text -> m a -> m a
collapsibleContainer label body = do
    let
        bodyClasses :: Bool -> T.Text
        bodyClasses shown = bool "hidden" mempty shown

    toggled <- toggleButton label

    elDynClass "div" (bodyClasses <$> toggled) body

-- | Collapsible container with an image icon in the header.
collapsibleContainerWithImage ::
    (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) =>
    T.Text ->
    T.Text ->
    m a ->
    m a
collapsibleContainerWithImage imgSrc label body = do
    let
        bodyClasses :: Bool -> T.Text
        bodyClasses shown = bool "hidden" mempty shown

    toggled <- toggleButton' imgSrc label
    elDynClass "div" (bodyClasses <$> toggled) body

-- | Toggle button variant with an image icon and responsive text sizing.
toggleButton' :: (MonadFix m, DomBuilder t m, PostBuild t m, MonadHold t m) => T.Text -> T.Text -> m (Dynamic t Bool)
toggleButton' imgSrc label = do
    let
        classes :: Bool -> T.Text
        classes shown =
            "text-icon font-icon transform select-none "
                <> "transition-transform duration-300 ease-in-out "
                <> bool "rotate-180" mempty shown

    rec (labelEl, _) <- elClass' "div" "grid grid-cols-12 p-4" $ do
            elClass "div" $(classh' [colSpan .~~ 2]) $ imgClass imgSrc ""
            elClass "div" $(classh' [colSpan .~~ 9]) $ CR.textS $(classh' [text_size .|~ [Base, LG, XL, XL2, XL3]]) label
            elDynClass' "span" (classes <$> toggled) $
                dynText "expand_less"

        let toggleEv = domEvent Click labelEl
        toggled <- holdUniqDyn =<< toggle True toggleEv

    pure toggled

-- | Toggle button with an image that shows \"open\"\/\"close\" text.
openCloseButton :: (Template t m) => ImgSrc -> Color -> T.Text -> m (Event t ())
openCloseButton imgSrc tColor name = do
    buttonToggleBody (constDyn "pl-10") True $ \case
        True -> do
            CR.col [6] $ imgClass imgSrc $(classh' [custom .~ "rotate-180 inline-block", position .~~ centered])
            CR.divClass $(classh' [colSpan .|~ [6], position .~~ centered, custom .~ "inline-block"]) $ do
                CR.textS (classhUnsafe [text_color .~~ tColor]) $ "close" <&> name
        False -> do
            CR.col [6] $ imgClass imgSrc $(classh' [custom .~ "rotate-180 inline-block", position .~~ centered])
            CR.divClass $(classh' [colSpan .|~ [6], position .~~ centered, custom .~ "inline-block"]) $ do
                CR.textS (classhUnsafe [text_color .~~ tColor]) $ "open" <&> name
