{-# LANGUAGE OverloadedStrings #-}
-- | Partial templates for displaying error messages.
module Templates.Partials.Errors where

import Classh
import Classh.Reflex (textS)

import Templates.Types
import Reflex.Dom.Core
import Data.Text (Text)

-- | Render the given template only when the 'Dynamic' is 'Just'.
maybeDisplay
  :: Template t m
  => (a -> m ()) -- ^ How to render contents
  -> Dynamic t (Maybe a) -- ^ What to watch for
  -> m ()
maybeDisplay template val = dyn_ $ ffor val $ \case
  Nothing -> blank
  Just x -> template x

-- | Render the given template when the 'Event' fires.
displayOn :: Template t m => (a -> m ()) -> Event t a -> m ()
displayOn template ev = maybeDisplay template =<< holdDyn Nothing (Just <$> ev)

-- | Render an error message.
errorMessage :: Template t m => Text -> m ()
errorMessage t =
  elClass "div" $(classh' [mt .~~ TWSize 1, h .~~ twSize' 4]) $ textS "text-opacity-70" t
