{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Partials.Inputs
Description : Text input and file attachment components
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Input components for messages (single-line and textarea) and file
attachments. All inputs support clear events and attribute modification.

<<doc/inputs.svg>>
-}
module Templates.Partials.Inputs (
    -- * Shared styling

    -- | Common box-level CSS classes for inputs.
    commonClassesBox,
    -- | Common text-level CSS classes for inputs.
    commonClassesText,

    -- * Message inputs

    -- | Single-line message input element.
    messageInput',
    -- | Multi-line textarea message input.
    messageInput,

    -- * File inputs

    -- | File attachment input element.
    attachmentInput,

    -- * Internal

    -- | Default attribute map for message inputs.
    messageInputAttrs,
) where

import Classh as C
import Data.Default (Default (def))
import Data.Map as Map
import Data.Text as T
import Reflex.Dom.Core hiding (El)
import Templates.Types (InputEl, Template)

-- | Common box-level CSS classes for inputs.
commonClassesBox :: T.Text
commonClassesBox = $(classh' [mx .~~ TWSize 1, custom .~ "focus:outline-none bg-inset flex-grow"])

-- | Common text-level CSS classes for inputs (responsive sizing).
commonClassesText :: T.Text
commonClassesText = $(classh' [text_size .|~ [XL2, XL4], custom .~ "font-label"])

-- | Single-line message input element.
messageInput' ::
    (Template t m) =>
    Event t (Map AttributeName Text) ->
    Event t () ->
    m (InputEl t m)
messageInput' newAttributes clearEvent =
    inputElement $
        def
            & initialAttributes
            .~ messageInputAttrs
            & inputElementConfig_setValue
            .~ ("" <$ clearEvent)
            & (inputElementConfig_elementConfig . elementConfig_modifyAttributes)
            .~ ((fmap . fmap) Just newAttributes)

-- | Multi-line textarea message input.
messageInput ::
    (Template t m) =>
    Event t (Map AttributeName Text) ->
    Event t () ->
    m (TextAreaElement EventResult (DomBuilderSpace m) t)
messageInput newAttributes clearEvent =
    textAreaElement $
        def
            & initialAttributes
            .~ messageInputAttrs
            & textAreaElementConfig_setValue
            .~ ("" <$ clearEvent)
            & (textAreaElementConfig_elementConfig . elementConfig_modifyAttributes)
            .~ ((fmap . fmap) Just newAttributes)

-- | Default attribute map for message inputs.
messageInputAttrs :: Map.Map AttributeName T.Text
messageInputAttrs =
    "class" =: $(classh' [w .~~ TWSize_Full, p .~~ TWSize 4, custom .~ "text-2xl"])
        <> "placeholder" =: "Type your message"
        <> "type" =: "text"
        <> "rows" =: "2"

-- | File attachment input element.
attachmentInput ::
    (Template t m) =>
    Event t () ->
    m (InputEl t m)
attachmentInput clearEvent =
    divClass classhd $
        inputElement $
            def
                & initialAttributes
                .~ ("type" =: "file" <> "class" =: "")
                & inputElementConfig_setValue
                .~ ("" <$ clearEvent)
  where
    classhd =
        "focus:outline-none mx-1 font-facit font-label text-2xl bg-inset"
            <&> boxCSS (def & position .~ centeredOnly)
