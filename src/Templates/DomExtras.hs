{-# Language OverloadedStrings #-}
module Templates.DomExtras where

import Reflex.Dom.Core
import Data.Text as T

elStyle :: DomBuilder t m =>
           T.Text
        -> T.Text
        -> m a
        -> m a
elStyle etag styleString inner = elAttr etag ("style" =: styleString) inner

elDynStyle :: (PostBuild t m, DomBuilder t m) => T.Text -> Dynamic t T.Text -> m a -> m a
elDynStyle etag styleString inner = elDynAttr etag ((=:) "style" <$> styleString) inner
