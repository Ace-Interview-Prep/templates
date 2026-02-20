{-# LANGUAGE OverloadedStrings #-}

module Templates.Partials.Buttons where

import Templates.Types
import Classh as C
import Classh.Reflex as C
import Data.Text as T
import Reflex.Dom.Core
import Control.Lens ((%~), (.~))
import Data.Proxy
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



primaryButtonDyn :: (DomBuilder t m, PostBuild t m) => Dynamic t Bool -> Text -> m (Event t ())
primaryButtonDyn enabled buttonText = do
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
navyBlueButton = navyBlueButton' (C.hex "2E3A59") C.White

navyBlueButton' :: DomBuilder t m => C.Color -> C.Color -> Text -> m (Event t ())
navyBlueButton' bgCol txtCol buttonText = do
  (e, _) <- elClass' "button" boxCfg $ C.textS textCfg buttonText
  pure $ domEvent Click e
  where
    textCfg = C.classhUnsafe [ C.text_color .~~ txtCol
                             , C.text_font .~~ C.Font_Custom "Sarabun"
                             , C.text_weight .~~ C.Bold
                             ]
    boxCfg = C.classhUnsafe [ C.w .~~ C.TWSize_Full
                            , C.p .~~ C.TWSize 4
                            , C.bgColor .~~ bgCol
                            , C.br .~~ C.R_3Xl
                            , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                                            , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                                            ]
                            , C.border . C.outline .~ [("focus", C.Outline_None)]
                            , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
                            , C.border . C.ring . C.ringOpacity .~~ 50
                            , C.custom .~ "ring-primary \
                                          \ hover:bg-primary-rich active:bg-primary-desaturated \
                                          \ transform hover:scale-105 active:scale-95"
                            ]




-- FROM ACE

primaryButtonImageDyn
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => Dynamic t Text
  -> Height
  -> Width
  -> m (Event t ())
primaryButtonImageDyn = primaryButtonImageDyn' (C.hex "00B9DA") C.White

primaryButtonImageDyn'
  :: ( PostBuild t m
     , DomBuilder t m
     )
  => C.Color
  -> C.Color
  -> Dynamic t Text
  -> Height
  -> Width
  -> m (Event t ())
primaryButtonImageDyn' bgCol txtCol dynImageHref height width = do
  (e, _) <- elClass' "button" boxCfg $
    elDynAttr "img" imgAttrs blank
  pure $ domEvent Click e
  where
    otherImgAttrs = "height" =: height <> "width" =: width <> "class" =: "block mx-auto"
    imgAttrs = (\src -> otherImgAttrs <> "src" =: src) <$> dynImageHref
    boxCfg = C.classhUnsafe [ C.w .~~ C.TWSize_Full
                            , C.p .~~ C.TWSize 4
                            , C.mt .~~ C.TWSize 16
                            , C.bgColor .~~ bgCol
                            , C.br .~~ C.R_Xl
                            , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                                            , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                                            ]
                            , C.border . C.outline .~ [("focus", C.Outline_None)]
                            , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
                            , C.border . C.ring . C.ringOpacity .~~ 50
                            , C.custom .~ "font-[Sarabun] font-bold text-white text-body text-center \
                                          \ ring-primary \
                                          \ hover:bg-primary-rich active:bg-primary-desaturated \
                                          \ transform hover:scale-105 active:scale-95"
                            ]



type Height = Text
type Width = Text
primaryButtonImage :: DomBuilder t m => Text -> Height -> Width -> m (Event t ())
primaryButtonImage = primaryButtonImage' (C.hex "00B9DA") C.White

primaryButtonImage' :: DomBuilder t m => C.Color -> C.Color -> Text -> Height -> Width -> m (Event t ())
primaryButtonImage' bgCol _ imageHref height width = do
  (e, _) <- elClass' "button" (primaryButtonBoxCfg bgCol) $
    elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
  pure $ domEvent Click e

primaryButtonImageText :: DomBuilder t m => Text -> Height -> Width -> Text -> m (Event t ())
primaryButtonImageText = primaryButtonImageText' (C.hex "00B9DA") C.White

primaryButtonImageText' :: DomBuilder t m => C.Color -> C.Color -> Text -> Height -> Width -> Text -> m (Event t ())
primaryButtonImageText' bgCol _ imageHref height width bottomText = do
  (e, _) <- elClass' "button" (primaryButtonBoxCfg bgCol) $ do
    elAttr "img" ("src" =: imageHref <> "height" =: height <> "width" =: width <> "class" =: "block mx-auto") blank
    text bottomText
  pure $ domEvent Click e

primaryButtonBoxCfg :: C.Color -> Text
primaryButtonBoxCfg bgCol = C.classhUnsafe [ C.w .~~ C.TWSize_Full
                                           , C.p .~~ C.TWSize 4
                                           , C.mt .~~ C.TWSize 16
                                           , C.bgColor .~~ bgCol
                                           , C.br .~~ C.R_Xl
                                           , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                                                           , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                                                           ]
                                           , C.border . C.outline .~ [("focus", C.Outline_None)]
                                           , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
                                           , C.border . C.ring . C.ringOpacity .~~ 50
                                           , C.custom .~ "font-[Sarabun] font-bold text-white text-body text-center \
                                                           \ ring-primary \
                                                           \ hover:bg-primary-rich active:bg-primary-desaturated \
                                                           \ transform hover:scale-105 active:scale-95"
                                           ]


primaryButtonSized :: DomBuilder t m => TWSize -> TWSize -> Text -> m (Event t ())
primaryButtonSized = primaryButtonSized' (C.hex "00B9DA") C.White

primaryButtonSized' :: DomBuilder t m => C.Color -> C.Color -> TWSize -> TWSize -> Text -> m (Event t ())
primaryButtonSized' bgCol _ height width buttonText = do
  (e, _) <- elAttr' "button" ("class" =: classCfg <> "name" =: name) $ text buttonText
  pure $ domEvent Click e
  where
    classCfg = primaryClass bgCol <&> C.classhUnsafe [C.py .~~ height, C.px .~~ width]
    name = T.filter (\c -> isAlphaNum c || isSpace c ) buttonText


primaryButton :: DomBuilder t m => Text -> m (Event t ())
primaryButton = primaryButton' (C.hex "00B9DA") C.White

primaryButton' :: DomBuilder t m => C.Color -> C.Color -> Text -> m (Event t ())
primaryButton' bgCol _ buttonText = do
  (e, _) <- elAttr' "button" ("class" =: classCfg <> "name" =: name) $ text buttonText
  pure $ domEvent Click e
  where
    classCfg = primaryClass bgCol <&> C.classhUnsafe [C.py .~~ C.TWSize 4, C.px .~~ C.TWSize 8]
    name = T.filter (\c -> isAlphaNum c || isSpace c ) buttonText

primaryClass :: C.Color -> Text
primaryClass bgCol = C.classhUnsafe [ C.bgColor .~~ bgCol
                                    , C.br .~~ C.R_Xl
                                    , C.shadow .~^ [("def", C.noTransition C.Shadow_Md)
                                                    , ("hover", C.Shadow_Md `C.withTransition` C.Duration_300)
                                                    ]
                                    , C.border . C.outline .~ [("focus", C.Outline_None)]
                                    , C.border . C.ring . C.ringWidth .~ [("focus", C.noTransition C.Ring_4)]
                                    , C.border . C.ring . C.ringOpacity .~~ 50
                                    , C.custom .~ "font-[Sarabun] font-bold text-white text-body text-center \
                                                    \ ring-primary \
                                                    \ hover:bg-primary-rich active:bg-primary-desaturated \
                                                    \ transform hover:scale-105 active:scale-95 \
                                                    \ whitespace-nowrap inline-block \
                                                    \ min-[0px]:text-xs md:text-lg"
                                    ]

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


-- | Stop event propogation on click
greedyButton :: forall t m a. DomBuilder t m => m a -> m (Event t (), a)
greedyButton c = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
            & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (const stopPropagation)
  (e, a) <- element "button" cfg c
  return (domEvent Click e, a)

greedyButton_ :: forall t m a. DomBuilder t m => m a -> m (Event t ())
greedyButton_ c = fmap fst $ greedyButton c
