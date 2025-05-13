{-# LANGUAGE OverloadedStrings #-}
module Templates.Partials.Inputs where

import Reflex.Dom.Core hiding (El)
import Templates.Types
import Data.Text as T
import Data.Map as Map

import Classh as C

commonClassesBox :: T.Text
commonClassesBox = $(classh' [mx .~~ TWSize 1, custom .~ "focus:outline-none bg-inset flex-grow" ]) 

commonClassesText :: T.Text
commonClassesText = $(classh' [text_size .|~ [XL2, XL4], custom .~ "font-label" ])

messageInput'
  :: Template t m
  => Event t (Map AttributeName Text)
  -> Event t ()
  -> m (InputEl t m)
messageInput' newAttributes clearEvent = inputElement $ def
  & initialAttributes .~ messageInputAttrs
  & inputElementConfig_setValue .~ ("" <$ clearEvent)
  & (inputElementConfig_elementConfig . elementConfig_modifyAttributes) .~ ((fmap.fmap) Just newAttributes)

messageInput
  :: Template t m
  => Event t (Map AttributeName Text)
  -> Event t ()
  -> m (TextAreaElement EventResult (DomBuilderSpace m) t)
messageInput newAttributes clearEvent = textAreaElement $ def
  & initialAttributes .~ messageInputAttrs
  & textAreaElementConfig_setValue .~ ("" <$ clearEvent)
  & (textAreaElementConfig_elementConfig . elementConfig_modifyAttributes) .~ ((fmap.fmap) Just newAttributes)


messageInputAttrs ::  Map.Map AttributeName T.Text
messageInputAttrs =
  "class" =: $(classh' [w .~~ TWSize_Full, p .~~ TWSize 4, custom .~ "text-2xl"] )
  <> "placeholder" =: "Type your message"
  <> "type" =: "text"
  <> "rows" =: "2"

attachmentInput
  :: Template t m
  => Event t ()
  -> m (InputEl t m)
attachmentInput clearEvent =
  divClass classhd $
    inputElement $ def
      & initialAttributes .~ ("type" =: "file" <> "class" =: "")
      & inputElementConfig_setValue .~ ("" <$ clearEvent)
  where
    classhd = "focus:outline-none mx-1 font-facit font-label text-2xl bg-inset"
      <&> boxCSS (def & position .~ centeredOnly ) 
