{-# LANGUAGE OverloadedStrings #-}
module Templates.Types
  ( module Templates.Types
  ) where

import Reflex.Dom.Core hiding (El)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)

type Template t m = (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)

type InputEl t m = InputElement EventResult (DomBuilderSpace m) t

type TextAreaEl t m = TextAreaElement EventResult (DomBuilderSpace m) t 

type El t m = Element EventResult (DomBuilderSpace m) t

type ImgSrc = Text
