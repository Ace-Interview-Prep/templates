module Templates.Partials.Image where

import Templates.Types
import Reflex.Dom.Core
import System.FilePath (takeBaseName)
import qualified Data.Map as Map
import qualified Data.Text as T

mkBasicName :: T.Text -> T.Text 
mkBasicName = T.replace (T.pack "-") (T.pack " ") . T.pack . takeBaseName . T.unpack 
  
img :: DomBuilder t m => ImgSrc -> m ()
img src = elAttr (T.pack "img") ((T.pack "src") =: src <> (T.pack "alt") =: mkBasicName src) blank

imgAttr :: DomBuilder t m => ImgSrc -> Map.Map T.Text T.Text -> m ()
imgAttr src atrs = elAttr (T.pack "img") ((T.pack "src") =: src <> atrs <> maybeAlt) blank
  where
    maybeAlt = case Map.lookup (T.pack "alt") atrs of
      Just _ -> mempty
      Nothing -> (T.pack "alt") =: mkBasicName src

imgClass :: DomBuilder t m => ImgSrc -> T.Text -> m ()
imgClass src class' = elAttr (T.pack "img") ((T.pack "src") =: src <> (T.pack "class") =: class' <> (T.pack "alt") =: mkBasicName src) blank
