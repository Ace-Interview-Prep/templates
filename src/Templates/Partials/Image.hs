module Templates.Partials.Image where

import Templates.Types
import Reflex.Dom.Core
import qualified Data.Map as Map
import qualified Data.Text as T

img :: DomBuilder t m => ImgSrc -> m ()
img src = elAttr (T.pack "img") ((T.pack "src") =: src) blank

imgAttr :: DomBuilder t m => ImgSrc -> Map.Map T.Text T.Text -> m ()
imgAttr src atrs = elAttr (T.pack "img") ((T.pack "src") =: src <> atrs) blank

imgClass :: DomBuilder t m => ImgSrc -> T.Text -> m ()
imgClass src class' = elAttr (T.pack "img") ((T.pack "src") =: src <> (T.pack "class") =: class') blank
