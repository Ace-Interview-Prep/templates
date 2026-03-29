{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Modal where

import Classh as C
import Classh.Reflex as C
import Templates.Partials.Image
import Templates.Types
import Reflex.Dom.Core
import Control.Monad.Fix
import Data.Text as T


-- modal :: ( DomBuilder t m
--          , MonadFix m
--          , MonadHold t m
--          , PostBuild t m
--          )
--   => ImgSrc
--   -> Event t ()
--   -> m a
--   -> m (Event t a)
-- modal = modal'
--   (C.solidColorOpacity C.Black 40)  -- overlay background
--   (C.solidColor (C.Slate C.C900))   -- content background (dark navy)
--   (C.color C.Black)                 -- border color
--   (C.color C.White)                 -- text color
--   (C.color (C.Rose C.C500))         -- close button color

modal'
  :: ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     )
  => C.GradientColor    -- ^ Overlay background color (with opacity)
  -> C.GradientColor    -- ^ Content background color
  -> C.ColorWithOpacity -- ^ Border color
  -> C.ColorWithOpacity -- ^ Text color
  -> C.ColorWithOpacity -- ^ Close button icon color
  -> ImgSrc
  -> Event t ()
  -> m a
  -> m (Event t a)
modal' overlayBg contentBg borderCol txtCol closeBtnCol xButtonImgSrc open modalDom = mdo
  -- Overlay positioning must remain as inline style (no Classh for position:fixed, z-index, etc.)
  let styleBase = "position:fixed;z-index:9999;padding-top:100px;left:0;top:0;width:100%;height:100%;overflow:auto;"
      hideModal = ("style" =: ("display:none;visibility:hidden;" <> styleBase) <> "class" =: overlayClass)
      showModal = ("style" =: ("display:block;visibility:visible;" <> styleBase) <> "class" =: overlayClass)
  modalAttrs <- holdDyn hideModal $ mergeWith const [showModal <$ open, hideModal <$ close]
  close <- elDynAttr "div" modalAttrs $ do
    elAttr "div" ("style" =: "margin:auto;width:80%;" <> "class" =: contentClass) $ do
      e <- fmap fst $ elClass' "span" closeSpanClass $ do
        imgAttr xButtonImgSrc ("height" =: "30px" <> "width" =: "30px")
      x' <- modalDom
      pure $ x' <$ (domEvent Click e)
  pure close
  where
    overlayClass = C.classhUnsafe [ C.bgColor .~~ overlayBg ]
    contentClass = C.classhUnsafe
      [ C.bgColor .~~ contentBg
      , C.p .~~ C.TWSize 5  -- padding:20px ≈ p-5
      , C.border . C.bStyle .~~ C.BDouble
      , C.br .~~ C.R_Md
      , C.bw .~~ C.B8
      , C.bc .~~ borderCol
      , C.custom .~ "text-base"  -- text-base for font sizing
      ] <> " " <> C.classhUnsafe [ C.text_color .~~ txtCol ]
    closeSpanClass = C.classhUnsafe
      [ C.pl .~~ C.TWSize 5
      , C.ml .~~ C.twSize' 5
      , C.pb .~~ C.TWSize 4
      , C.custom .~ "grid justify-items-end"
      ] <> " " <> C.classhUnsafe [ C.text_color .~~ closeBtnCol ]
