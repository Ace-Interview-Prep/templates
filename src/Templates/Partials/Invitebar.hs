{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : Templates.Partials.Invitebar
Description : Email invite input with validation feedback
Copyright   : (c) Galen Sprout, 2024
License     : MIT
Maintainer  : galen.sprout@gmail.com

An email invitation input bar with a send button and validation
feedback. The send button enables when the input is non-empty, and
feedback text appears after sending.

<<doc/invitebar.svg>>
-}
module Templates.Partials.Invitebar (
    -- * Email validation (stub)

    {- | Parse\/validate an email address. Currently a stub that always
    returns @Right True@.
    -}
    emailParse,
    -- | Generate feedback text from an email validation result.
    emailText,

    -- * Invite bar

    {- | Email invite input with send button and validation feedback.
    Returns the input element and the send button click event.
    -}
    invitebar,
) where

import Classh as C
import Classh.Reflex (textS)
import Control.Monad.Fix (MonadFix)
import Data.Text (Text)
import qualified Data.Text as T
import Reflex.Dom.Core
import Templates.Partials.Buttons (iconButton')
import Templates.Types (InputEl)

{- | Parse\/validate an email address.
Currently a stub that always returns @Right True@.
-}
emailParse :: a -> Either T.Text Bool
emailParse _ = Right True

-- | Generate feedback text from an email address based on validation.
emailText :: T.Text -> T.Text
emailText e = case emailParse e of
    Right True -> "Invitation sent to " <> e
    Right False -> "Email must be @aceinterviewprep.io"
    Left _ -> "Invalid email format"

-- | Email invite input bar with send button and validation feedback.
invitebar :: (PostBuild t m, DomBuilder t m, MonadHold t m, MonadFix m) => Text -> m (InputEl t m, Event t ())
invitebar placeholder = do
    rec let emailClass e = ffor (emailParse <$> e) $ \case
                Right True -> "border-rose-600 text-rose-600"
                _ -> "border-rose-600 text-rose-600"
        invbar@(invInput, invButton) <-
            elClass
                "div"
                $( classh'
                    [ my .~~ TWSize 2
                    , w .~~ TWSize_Full
                    , custom .~ "shadow-button flex flex-row"
                    , bgColor .~~ White
                    , br .~~ R_Normal
                    ]
                 )
                $ do
                    elClass "button" $(classh' [pl .~~ TWSize 2]) $ textS "font-icon text-icon" "email"
                    invInput' <-
                        inputElement $
                            def
                                & initialAttributes
                                .~ ( "class"
                                        =: $( classh'
                                                [ w .~~ TWSize_Full
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

        toEnable <-
            holdDyn False $
                const True
                    <$> gate
                        (current $ T.null <$> _inputElement_value invInput)
                        (_inputElement_input invInput)

    toHide <-
        holdDyn "hidden " $
            const mempty
                <$> gate
                    (current $ not . T.null <$> _inputElement_value invInput)
                    invButton

    feedback <-
        holdDyn mempty $
            emailText
                <$> tag
                    (current $ _inputElement_value invInput)
                    invButton

    let dynClass =
            fmap
                ((<>) $(classh' [pl .~~ TWSize 1, pt .~~ TWSize 0, pb .~~ TWSize 1, pr .~~ TWSize 3, custom .~ "text-center"]))
                $ toHide
                    <> (emailClass $ _inputElement_value invInput)

    elDynClass "div" dynClass $
        dynText feedback

    return invbar
