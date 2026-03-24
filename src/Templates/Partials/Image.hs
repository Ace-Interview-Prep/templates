{- |
Module      : Templates.Partials.Image
Description : Image elements with auto-generated alt text
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Provides image element helpers that automatically generate accessible
@alt@ text from the image filename using 'mkBasicName'.

<<doc/image.svg>>
-}
module Templates.Partials.Image (
    -- * Alt text generation

    {- | Generate human-readable alt text from an image file path.
    Strips the directory and extension, then replaces dashes with spaces.

    @
    mkBasicName \"\/assets\/profile-photo.png\" == \"profile photo\"
    @
    -}
    mkBasicName,

    -- * Image elements

    -- | Simple @\<img\>@ element with auto-generated alt text.
    img,
    {- | Image element with additional attributes. If @alt@ is already
    present in the attribute map, it is preserved (not overwritten).
    -}
    imgAttr,
    -- | Image element with a CSS class and auto-generated alt text.
    imgClass,
) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Reflex.Dom.Core (DomBuilder, blank, elAttr, (=:))
import System.FilePath (takeBaseName)
import Templates.Types (ImgSrc)

{- | Generate human-readable alt text from an image file path.
Strips the directory and extension, then replaces dashes with spaces.
-}
mkBasicName :: T.Text -> T.Text
mkBasicName = T.replace (T.pack "-") (T.pack " ") . T.pack . takeBaseName . T.unpack

-- | Simple @\<img\>@ element with auto-generated alt text from the filename.
img :: (DomBuilder t m) => ImgSrc -> m ()
img src = elAttr (T.pack "img") ((T.pack "src") =: src <> (T.pack "alt") =: mkBasicName src) blank

{- | Image element with additional attributes. If @alt@ is already present
in the attribute map, it is preserved rather than overwritten.
-}
imgAttr :: (DomBuilder t m) => ImgSrc -> Map.Map T.Text T.Text -> m ()
imgAttr src atrs = elAttr (T.pack "img") ((T.pack "src") =: src <> atrs <> maybeAlt) blank
  where
    maybeAlt = case Map.lookup (T.pack "alt") atrs of
        Just _ -> mempty
        Nothing -> (T.pack "alt") =: mkBasicName src

-- | Image element with a CSS class and auto-generated alt text.
imgClass :: (DomBuilder t m) => ImgSrc -> T.Text -> m ()
imgClass src class' = elAttr (T.pack "img") ((T.pack "src") =: src <> (T.pack "class") =: class' <> (T.pack "alt") =: mkBasicName src) blank
