{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Modal where

import Templates.Partials.Image
import Templates.Types
import Reflex.Dom.Core
import Control.Monad.Fix
import Data.Text as T


modal :: ( DomBuilder t m
         , MonadFix m
         , MonadHold t m
         , PostBuild t m
         )
  => ImgSrc
  -> Event t ()
  -> m a
  -> m (Event t a)
modal xButtonImgSrc open modalDom = mdo
  let styleBase = "position:fixed;z-index:20;padding-top:100px;left:0;top:0;width:100%;height:100%;overflow:auto;background-color:rgb(0,0,0);background-color:rgba(0,0,0,0.4);"
      hideModal = ("style" =: ("display:none;" <> styleBase))
      showModal = ("style" =: ("display:block;" <> styleBase))
  modalAttrs <- holdDyn hideModal $ mergeWith const [showModal <$ open, hideModal <$ close]
  close <- elDynAttr "div" modalAttrs $ do
    elAttr "div" ("style" =: "background-color:#00004D;margin:auto;padding:20px;width:80%;color:white;"
                  <> "class" =: "border-double rounded-md border-8 border-black text-base") $ do
      e <- fmap fst $ elClass' "span" "pl-5 ml-5 pb-4 text-rose-500 grid justify-items-end" $ do
        imgAttr xButtonImgSrc ("height" =: "30px" <> "width" =: "30px")
      x' <- modalDom
      pure $ x' <$ (domEvent Click e)
  pure close
