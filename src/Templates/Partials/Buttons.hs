{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Buttons where

import Templates.Types 
import Classh as C
import Classh.Reflex as C
import Data.Text as T 
import Reflex.Dom.Core
import Data.Char (isAlphaNum, isSpace)

iconButton :: DomBuilder t m => Text -> m (Event t ())
iconButton icon = do
  (e, _) <- elClass' "button" classes $ text icon
  pure $ domEvent Click e
  where
    classes = "focus:outline-none bg-primary rounded p-2.5 w-[50%] font-icon text-icon text-white leading-none shadow-button h-[95%]"
  
iconButton' :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m (Event t ())
iconButton' enabled icon = do
  let attrs = ffor enabled $ \b -> if b then "class" =: classes else "class" =: classes <> "disabled" =: "1"
  (e, _) <- elDynAttr' "button" attrs $ text icon
  pure $ domEvent Click e
  where
    classes = "focus:outline-none flex-shrink-0 bg-primary rounded p-2.5 font-icon text-icon text-white leading-none shadow-button"



primaryButton' :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m (Event t ())
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

secondaryIconButton :: DomBuilder t m => Text -> Text -> m (Event t ())
secondaryIconButton cs icon = do
  (e, _) <- elClass' "button" classes $
    elClass "div" "font-icon leading-none text-icon text-primary-dark" $ text icon
  pure $ domEvent Click e
  where
    classes =
      "focus:outline-none rounded border border-metaline \
      \ focus:ring-4 ring-primary ring-opacity-50 \
      \ p-2.5 flex-shrink-0 bg-primary-light " <> cs


secondaryButton :: DomBuilder t m => Text -> Text -> m (Event t ())
secondaryButton cs label = do
  (e, _) <- elClass' "button" classes $
    text label
  pure $ domEvent Click e
  where
    classes =
      "w-full p-2.5 leading-none text-center rounded border border-metaline \
      \ bg-primary-light text-primary-darker font-bold font-facit focus:outline-none \
      \ focus:ring-4 ring-primary ring-opacity-50 " <> cs

sendButton :: DomBuilder t m => m (Event t ())
sendButton = iconButton "send"


navyBlueButton :: DomBuilder t m => Text -> m (Event t ())
navyBlueButton buttonText = do
  (e, _) <- elClass' "button" other $ C.textS textCfg' buttonText
  pure $ domEvent Click e
  where
    textCfg' = $(C.classh' [ C.text_color C..~~ C.White, C.text_font C..~~ C.Font_Custom "Sarabun"
                           , C.text_weight C..~~ C.Bold
                           ])
    other =
      "focus:outline-none w-full p-4 shadow-button bg-[#2E3A59] \
      \ rounded-3xl hover:bg-primary-rich active:bg-primary-desaturated \
      \ focus:ring-4 ring-primary ring-opacity-50 \
      \ transition-all duration-300 ease-in-out \ 
      \ transform hover:scale-105 active:scale-95 \
      \ hover:shadow-md active:shadow-lg"




-- FROM ACE

primaryButtonImageDyn
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Text
  -> Height
  -> Width
  -> m (Event t ())
primaryButtonImageDyn dynImageHref height width = do
  (e, _) <- elClass' "button" classes $
    elDynAttr "img"  imgAttrs blank
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



type Height = Text
type Width = Text
primaryButtonImage :: DomBuilder t m => Text -> Height -> Width -> m (Event t ())
primaryButtonImage imageHref height width = do
  (e, _) <- elClass' "button" classes $
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

primaryButtonImageText :: DomBuilder t m => Text -> Height -> Width -> Text -> m (Event t ())
primaryButtonImageText imageHref height width bottomText = do
  (e, _) <- elClass' "button" classes $ do
    elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
    text bottomText -- TODO: replace with styledText
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


primaryButtonSized :: DomBuilder t m => TWSize -> TWSize -> Text -> m (Event t ())
primaryButtonSized height width buttonText = do
  (e, _) <- elAttr' "button" ("class" =: classTW <> "name" =: name) $ text buttonText
  pure $ domEvent Click e
  where
    classTW = primaryClass <&> ("py-" <> showTW height) <&> ("px-" <> showTW width)
    -- for testing / selenium mainly
    name = T.filter (\c -> isAlphaNum c || isSpace c ) buttonText


primaryButton :: DomBuilder t m => Text -> m (Event t ())
primaryButton buttonText = do
  (e, _) <- elAttr' "button" ("class" =: (primaryClass <&> "py-4 px-8") <> "name" =: name) $ text buttonText
  pure $ domEvent Click e
  where
    -- for testing / selenium mainly
    name = T.filter (\c -> isAlphaNum c || isSpace c ) buttonText
 
primaryClass =
  "focus:outline-none shadow-button bg-[#00B9DA] \
  \ font-[Sarabun] font-bold text-white text-body text-center rounded-xl \
  \ hover:bg-primary-rich active:bg-primary-desaturated \
  \ focus:ring-4 ring-primary ring-opacity-50 \
  \ transition-all duration-300 ease-in-out \
  \ transform hover:scale-105 active:scale-95 \
  \ whitespace-nowrap inline-block \
  \ hover:shadow-md active:shadow-lg min-[0px]:text-xs md:text-lg"

example :: Template t m => m (Event t ())
example = buttonToggleBody "" True $ \case
  True -> text "hey"
  False -> text "Hello"

-- | Toggles between 2 inner bodies based on its clicks 
buttonToggleBody :: Template t m => Dynamic t T.Text -> Bool -> (Bool -> m a) -> m (Event t ())
buttonToggleBody buttonClasses start dynDom = mdo
  (e, _) <- elDynClass' "button" buttonClasses $ dyn $ ffor booly $ dynDom
  let clk = domEvent Click e
  booly <- toggle start clk
  pure clk

