{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.DomExtras
Description : Convenience wrappers for inline-styled DOM elements
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Utility functions for creating DOM elements with inline @style@ attributes.
-}
module Templates.DomExtras (
    -- * Static inline styles

    -- | Create an element with a static inline style string.
    elStyle,

    -- * Dynamic inline styles

    -- | Create an element with a dynamic inline style string.
    elDynStyle,
) where

import Data.Text (Text)
import Reflex.Dom.Core (DomBuilder, Dynamic, PostBuild, elAttr, elDynAttr, (=:))

-- | Create an element with a static inline @style@ attribute.
elStyle ::
    (DomBuilder t m) =>
    Text ->
    Text ->
    m a ->
    m a
elStyle etag styleString inner = elAttr etag ("style" =: styleString) inner

-- | Create an element with a dynamic inline @style@ attribute.
elDynStyle :: (PostBuild t m, DomBuilder t m) => Text -> Dynamic t Text -> m a -> m a
elDynStyle etag styleString inner = elDynAttr etag ((=:) "style" <$> styleString) inner
