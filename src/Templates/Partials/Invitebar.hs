{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
module Templates.Partials.Invitebar where

import Classh
import Classh.Reflex (textS)

import Data.Default
import Data.Text (Text)
import Data.Text as T
import Control.Monad.Fix (MonadFix)

import Reflex.Dom.Core
import Templates.Types 
import Templates.Partials.Buttons
--import Common.Utils

emailParse :: a -> Either T.Text Bool
emailParse _ = Right True

-- | Builds an input bar for emails, it returns both the input and
-- the button that sends it.
invitebar :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m) => Text -> m (InputEl t m, Event t ())
invitebar placeholder = do
  rec
    let emailClass e = ffor (emailParse <$> e) $ \case
          Right True -> "border-rose-600 text-rose-600"
          _ -> "border-rose-600 text-rose-600"
        emailText e = case emailParse e of
          Right True -> "Invitation sent to " <> e
          -- TODO: remove
          Right False -> "Email must be @aceinterviewprep.io"
          Left _ -> "Invalid email format"
    invbar@(invInput, invButton) <- elClass "div" $(classh' [my .~~ TWSize 2
                                                            , w .~~ TWSize_Full
                                                            , custom .~ "shadow-button flex flex-row"
                                                            , bgColor .~~ White
                                                            , br .~~ R_Normal
                                                            ]) $ do
      elClass "button" $(classh' [ pl .~~ TWSize 2 ]) $ textS "font-icon text-icon" "email"
      invInput' <- inputElement $ def
        & initialAttributes .~
          ("class" =:
            $(classh' [ w .~~ TWSize_Full
                      , h .~~ TWSize_Full
                      , bgColor .~~ Transparent
                      , px .~~ TWSize 1
                      , pt .~~ TWSize 3
                      , pb .~~ TWSize 3
                      , pr .~~ TWSize 2
                      , custom .~ "focus:outline-none flex-grow placeholder-light font-label"
                      ]
             )            
          <> "placeholder" =: placeholder
          <> "type" =: "email"
          )
      invButton' <- iconButton' toEnable "send"

      return (invInput', invButton')

    toEnable <- holdDyn False $
      const True <$> gate 
        (current $ T.null <$> _inputElement_value invInput)
        (_inputElement_input invInput)

  toHide <- holdDyn "hidden " $
    const mempty <$> gate 
      (current $ not . T.null <$> _inputElement_value invInput)
      invButton

  feedback <- holdDyn mempty $
    emailText <$> tag 
      (current $ _inputElement_value invInput)
      invButton

  let
    dynClass = 
      fmap ( (<>) $(classh' [pl .~~ TWSize 1, pt .~~ TWSize 0, pb .~~ TWSize 1, pr .~~ TWSize 3, custom .~ "text-center"]))
      $ toHide
      <> (emailClass $ _inputElement_value invInput)

  elDynClass "div" dynClass
      $ dynText feedback

  return invbar
