{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Templates.Partials.Buttons
Description : Button component variants for reflex-dom
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

A collection of button components with different visual styles. Includes
primary, secondary, icon, navy-blue, image, sized, toggle, and
greedy (stop-propagation) variants.

<<doc/buttons.svg>>
-}
module Templates.Partials.Buttons (
    -- * Icon buttons

    -- | Icon-only button with primary styling.
    iconButton,
    -- | Icon-only button that can be enabled\/disabled dynamically.
    iconButton',
    -- | Secondary-styled button with an icon.
    secondaryIconButton,

    -- * Primary buttons

    -- | Primary action button with dynamic enable\/disable.
    primaryButton',
    -- | Full-width primary button.
    primaryButton,
    -- | Primary button with configurable padding via 'TWSize'.
    primaryButtonSized,
    -- | Navy blue themed button with Sarabun font.
    navyBlueButton,

    -- * Image buttons

    -- | Primary button with a dynamic image.
    primaryButtonImageDyn,
    -- | Primary button with a static image.
    primaryButtonImage,
    -- | Primary button with image and bottom label text.
    primaryButtonImageText,

    -- * Secondary buttons

    -- | Secondary-styled button with custom classes.
    secondaryButton,
    -- | Send icon button (alias for @iconButton \"send\"@).
    sendButton,

    -- * Toggle and special buttons

    -- | Button that toggles between two inner body renderings on click.
    buttonToggleBody,
    -- | Button that stops click event propagation.
    greedyButton,
    -- | Variant of 'greedyButton' that discards the inner widget result.
    greedyButton_,

    -- * Re-exports

    -- | Dimension type alias for image buttons.
    Height,
    -- | Dimension type alias for image buttons.
    Width,

    -- * Examples

    -- | Example usage of 'buttonToggleBody'.
    example,

    -- * Internal

    -- | Shared primary button class string.
    primaryClass,
) where

import Classh as C
import Classh.Reflex as CR
import Control.Lens ((%~))
import Data.Char (isAlphaNum, isSpace)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text as T
import Reflex.Dom.Core
import Templates.Types (Template)

-- | Icon-only button with primary styling.
iconButton :: (DomBuilder t m) => T.Text -> m (Event t ())
iconButton icon = do
    (e, _) <- elClass' "button" classes $ text icon
    pure $ domEvent Click e
  where
    classes = "focus:outline-none bg-primary rounded p-2.5 w-[50%] font-icon text-icon text-white leading-none shadow-button h-[95%]"

-- | Icon-only button that can be enabled\/disabled dynamically.
iconButton' :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> T.Text -> m (Event t ())
iconButton' enabled icon = do
    let attrs = ffor enabled $ \b -> if b then "class" =: classes else "class" =: classes <> "disabled" =: "1"
    (e, _) <- elDynAttr' "button" attrs $ text icon
    pure $ domEvent Click e
  where
    classes = "focus:outline-none flex-shrink-0 bg-primary rounded p-2.5 font-icon text-icon text-white leading-none shadow-button"

-- | Primary action button with dynamic enable\/disable.
primaryButton' :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> T.Text -> m (Event t ())
primaryButton' enabled buttonText = do
    let attrs = ffor enabled $ \b -> if b then "class" =: classes else "class" =: classes <> "disabled" =: "1"
    (e, _) <- elDynAttr' "button" attrs $ text buttonText
    pure $ domEvent Click e
  where
    classes =
        "focus:outline-none w-full p-4 mt-12 shadow-button bg-primary \
        \ font-facit font-bold text-white text-body text-center rounded \
        \ hover:bg-primary-rich active:bg-primary-desaturated \
        \ focus:ring-4 ring-primary ring-opacity-50"

-- | Secondary-styled button with an icon.
secondaryIconButton :: (DomBuilder t m) => T.Text -> T.Text -> m (Event t ())
secondaryIconButton cs icon = do
    (e, _) <-
        elClass' "button" classes $
            elClass "div" "font-icon leading-none text-icon text-primary-dark" $
                text icon
    pure $ domEvent Click e
  where
    classes =
        "focus:outline-none rounded border border-metaline \
        \ focus:ring-4 ring-primary ring-opacity-50 \
        \ p-2.5 flex-shrink-0 bg-primary-light "
            <> cs

-- | Secondary-styled button with custom classes and a text label.
secondaryButton :: (DomBuilder t m) => T.Text -> T.Text -> m (Event t ())
secondaryButton cs label = do
    (e, _) <-
        elClass' "button" classes $
            text label
    pure $ domEvent Click e
  where
    classes =
        "w-full p-2.5 leading-none text-center rounded border border-metaline \
        \ bg-primary-light text-primary-darker font-bold font-facit focus:outline-none \
        \ focus:ring-4 ring-primary ring-opacity-50 "
            <> cs

-- | Send icon button — alias for @iconButton \"send\"@.
sendButton :: (DomBuilder t m) => m (Event t ())
sendButton = iconButton "send"

-- | Navy blue themed button with Sarabun font and scale animations.
navyBlueButton :: (DomBuilder t m) => T.Text -> m (Event t ())
navyBlueButton buttonText = do
    (e, _) <- elClass' "button" other $ CR.textS textCfg' buttonText
    pure $ domEvent Click e
  where
    textCfg' =
        $( C.classh'
            [ C.text_color C..~~ C.White
            , C.text_font C..~~ C.Font_Custom "Sarabun"
            , C.text_weight C..~~ C.Bold
            ]
         )
    other =
        "focus:outline-none w-full p-4 shadow-button bg-[#2E3A59] \
        \ rounded-3xl hover:bg-primary-rich active:bg-primary-desaturated \
        \ focus:ring-4 ring-primary ring-opacity-50 \
        \ transition-all duration-300 ease-in-out \
        \ transform hover:scale-105 active:scale-95 \
        \ hover:shadow-md active:shadow-lg"

-- | Primary button with a dynamic image source.
primaryButtonImageDyn ::
    (PostBuild t m, DomBuilder t m) =>
    Dynamic t T.Text ->
    Height ->
    Width ->
    m (Event t ())
primaryButtonImageDyn dynImageHref height width = do
    (e, _) <-
        elClass' "button" classes $
            elDynAttr "img" imgAttrs blank
    pure $ domEvent Click e
  where
    otherImgAttrs = "height" =: height <> "width" =: width <> "class" =: "block mx-auto"
    imgAttrs = (\src -> otherImgAttrs <> "src" =: src) <$> dynImageHref
    classes =
        "focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA] \
        \ font-[Sarabun] font-bold text-white text-body text-center rounded-xl \
        \ hover:bg-primary-rich active:bg-primary-desaturated \
        \ focus:ring-4 ring-primary ring-opacity-50 \
        \ transition-all duration-300 ease-in-out \
        \ transform hover:scale-105 active:scale-95 \
        \ hover:shadow-md active:shadow-lg"

-- | Dimension type alias for image buttons.
type Height = T.Text

-- | Dimension type alias for image buttons.
type Width = T.Text

-- | Primary button with a static image.
primaryButtonImage :: (DomBuilder t m) => T.Text -> Height -> Width -> m (Event t ())
primaryButtonImage imageHref height width = do
    (e, _) <-
        elClass' "button" classes $
            elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
    pure $ domEvent Click e
  where
    classes =
        "focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA] \
        \ font-[Sarabun] font-bold text-white text-body text-center rounded-xl \
        \ hover:bg-primary-rich active:bg-primary-desaturated \
        \ focus:ring-4 ring-primary ring-opacity-50 \
        \ transition-all duration-300 ease-in-out \
        \ transform hover:scale-105 active:scale-95 \
        \ hover:shadow-md active:shadow-lg"

-- | Primary button with an image and bottom label text.
primaryButtonImageText :: (DomBuilder t m) => T.Text -> Height -> Width -> T.Text -> m (Event t ())
primaryButtonImageText imageHref height width bottomText = do
    (e, _) <- elClass' "button" classes $ do
        elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
        text bottomText
    pure $ domEvent Click e
  where
    classes =
        "focus:outline-none w-full p-4 mt-16 shadow-button bg-[#00B9DA] \
        \ font-[Sarabun] font-bold text-white text-body text-center rounded-xl \
        \ hover:bg-primary-rich active:bg-primary-desaturated \
        \ focus:ring-4 ring-primary ring-opacity-50 \
        \ transition-all duration-300 ease-in-out \
        \ transform hover:scale-105 active:scale-95 \
        \ hover:shadow-md active:shadow-lg"

-- | Primary button with configurable padding via 'TWSize'.
primaryButtonSized :: (DomBuilder t m) => TWSize -> TWSize -> T.Text -> m (Event t ())
primaryButtonSized height width buttonText = do
    (e, _) <- elAttr' "button" ("class" =: classTW <> "name" =: name) $ text buttonText
    pure $ domEvent Click e
  where
    classTW = primaryClass <&> ("py-" <> showTW height) <&> ("px-" <> showTW width)
    -- for testing / selenium mainly
    name = T.filter (\c -> isAlphaNum c || isSpace c) buttonText

-- | Full-width primary button.
primaryButton :: (DomBuilder t m) => T.Text -> m (Event t ())
primaryButton buttonText = do
    (e, _) <- elAttr' "button" ("class" =: (primaryClass <&> "py-4 px-8") <> "name" =: name) $ text buttonText
    pure $ domEvent Click e
  where
    -- for testing / selenium mainly
    name = T.filter (\c -> isAlphaNum c || isSpace c) buttonText

-- | Shared primary button class string.
primaryClass :: T.Text
primaryClass =
    "focus:outline-none shadow-button bg-[#00B9DA] \
    \ font-[Sarabun] font-bold text-white text-body text-center rounded-xl \
    \ hover:bg-primary-rich active:bg-primary-desaturated \
    \ focus:ring-4 ring-primary ring-opacity-50 \
    \ transition-all duration-300 ease-in-out \
    \ transform hover:scale-105 active:scale-95 \
    \ whitespace-nowrap inline-block \
    \ hover:shadow-md active:shadow-lg min-[0px]:text-xs md:text-lg"

-- | Example usage of 'buttonToggleBody'.
example :: (Template t m) => m (Event t ())
example = buttonToggleBody "" True $ \case
    True -> text "hey"
    False -> text "Hello"

-- | Button that toggles between two inner body renderings on click.
buttonToggleBody :: (Template t m) => Dynamic t T.Text -> Bool -> (Bool -> m a) -> m (Event t ())
buttonToggleBody buttonClasses start dynDom = mdo
    (e, _) <- elDynClass' "button" buttonClasses $ dyn $ ffor booly $ dynDom
    let clk = domEvent Click e
    booly <- toggle start clk
    pure clk

{- | Button that stops click event propagation. Returns both the click
event and the inner widget result.
-}
greedyButton :: forall t m a. (DomBuilder t m) => m a -> m (Event t (), a)
greedyButton c = do
    let cfg =
            (def :: ElementConfig EventResult t (DomBuilderSpace m))
                & elementConfig_eventSpec
                %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
    (e, a) <- element "button" cfg c
    return (domEvent Click e, a)

-- | Variant of 'greedyButton' that discards the inner widget result.
greedyButton_ :: forall t m a. (DomBuilder t m) => m a -> m (Event t ())
greedyButton_ c = fmap fst $ greedyButton c
