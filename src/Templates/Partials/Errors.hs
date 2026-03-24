{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Partials.Errors
Description : Error and conditional display components
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

Components for conditionally rendering content based on 'Either',
'Maybe', and 'Event' values, plus a styled error message display.

<<doc/errors.svg>>
-}
module Templates.Partials.Errors (
    -- * Conditional display

    -- | Render one of two templates based on an 'Either' value.
    eitherDisplay,
    -- | Render a template only when the 'Dynamic' holds 'Just'.
    maybeDisplay,
    -- | Render a template when the 'Event' fires.
    displayOn,

    -- * Error messages

    -- | Display a styled error message.
    errorMessage,
) where

import Classh
import Classh.Reflex (textS)
import Data.Text (Text)
import Reflex.Dom.Core (Dynamic, Event, blank, dyn_, elClass, ffor, holdDyn)
import Templates.Types (Template)

{- | Render one of two templates depending on whether the 'Dynamic' holds
a 'Left' or 'Right' value.
-}
eitherDisplay ::
    (Template t m) =>
    -- | What to watch for
    Dynamic t (Either e a) ->
    -- | How to render the 'Right' contents
    (a -> m ()) ->
    -- | How to render the 'Left' error
    (e -> m ()) ->
    m ()
eitherDisplay dynEith template failtemplate = dyn_ $ ffor dynEith $ \case
    Left e -> failtemplate e
    Right x_ -> template x_

-- | Render the given template only when the 'Dynamic' is 'Just'.
maybeDisplay ::
    (Template t m) =>
    -- | How to render contents
    (a -> m ()) ->
    -- | What to watch for
    Dynamic t (Maybe a) ->
    m ()
maybeDisplay template val = dyn_ $ ffor val $ \case
    Nothing -> blank
    Just x -> template x

-- | Render the given template when the 'Event' fires.
displayOn :: (Template t m) => (a -> m ()) -> Event t a -> m ()
displayOn template ev = maybeDisplay template =<< holdDyn Nothing (Just <$> ev)

-- | Render an error message with styling.
errorMessage :: (Template t m) => Text -> m ()
errorMessage t =
    elClass "div" $(classh' [mt .~~ TWSize 1, h .~~ twSize' 4]) $ textS "text-opacity-70" t
