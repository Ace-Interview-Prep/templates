{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Partials.Modal
Description : Modal overlay component
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

A modal dialog that overlays the page with a semi-transparent backdrop.
Opens on an external event, closes when the close button (X image) is
clicked, and returns the inner widget's result as an event.

<<doc/modal.svg>>
-}
module Templates.Partials.Modal (
    -- * Modal

    {- | Display a modal overlay. The modal opens when the given event
    fires, and closes when the close button is clicked. Returns the
    inner widget's result as an event on close.
    -}
    modal,
) where

import Control.Monad.Fix (MonadFix)
import qualified Data.Text as T
import Reflex.Dom.Core
import Templates.Partials.Image (imgAttr)
import Templates.Types (ImgSrc)

-- | Display a modal overlay with a close button.
modal ::
    (DomBuilder t m, MonadFix m, MonadHold t m, PostBuild t m) =>
    -- | Image source for the close (X) button
    ImgSrc ->
    -- | Event that opens the modal
    Event t () ->
    -- | Inner widget to render inside the modal
    m a ->
    -- | Event carrying the inner widget's result, fired on close
    m (Event t a)
modal xButtonImgSrc open modalDom = mdo
    let styleBase = "position:fixed;z-index:20;padding-top:100px;left:0;top:0;width:100%;height:100%;overflow:auto;background-color:rgb(0,0,0);background-color:rgba(0,0,0,0.4);"
        hideModal = ("style" =: ("display:none;" <> styleBase))
        showModal = ("style" =: ("display:block;" <> styleBase))
    modalAttrs <- holdDyn hideModal $ mergeWith const [showModal <$ open, hideModal <$ close]
    close <- elDynAttr "div" modalAttrs $ do
        elAttr
            "div"
            ( "style" =: "background-color:#00004D;margin:auto;padding:20px;width:80%;color:white;"
                <> "class" =: "border-double rounded-md border-8 border-black text-base"
            )
            $ do
                e <- fmap fst $ elClass' "span" "pl-5 ml-5 pb-4 text-rose-500 grid justify-items-end" $ do
                    imgAttr xButtonImgSrc ("height" =: "30px" <> "width" =: "30px")
                x' <- modalDom
                pure $ x' <$ (domEvent Click e)
    pure close
