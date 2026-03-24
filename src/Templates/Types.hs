{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Types
Description : Core type aliases for the templates component library
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Common constraint aliases and element type synonyms used across all
template components.
-}
module Templates.Types (
    -- * Constraint aliases

    {- | The standard constraint bundle for components that need to build
    DOM elements, react to post-build events, hold state, and use
    @mdo@\/@rec@.
    -}
    Template,

    -- * Element type aliases

    -- | Shortened alias for 'InputElement'.
    InputEl,
    -- | Shortened alias for 'TextAreaElement'.
    TextAreaEl,
    -- | Shortened alias for 'Element'.
    El,

    -- * Value type aliases

    -- | Image source URL.
    ImgSrc,
) where

import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import Reflex.Dom.Core hiding (El)

-- | Standard constraint bundle for template components.
type Template t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)

-- | Shortened alias for 'InputElement'.
type InputEl t m = InputElement EventResult (DomBuilderSpace m) t

-- | Shortened alias for 'TextAreaElement'.
type TextAreaEl t m = TextAreaElement EventResult (DomBuilderSpace m) t

-- | Shortened alias for 'Element'.
type El t m = Element EventResult (DomBuilderSpace m) t

-- | Image source URL.
type ImgSrc = Text
