{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog (
    Gen,
    Property,
    assert,
    forAll,
    property,
    (===),
 )
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.Hedgehog (testPropertyNamed)

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (takeBaseName)

-- ---------------------------------------------------------------------------
-- Pure functions extracted from templates (no reflex-dom dependency)
-- ---------------------------------------------------------------------------

-- | From Templates.Partials.Image: generates alt text from image filename.
mkBasicName :: Text -> Text
mkBasicName = T.replace (T.pack "-") (T.pack " ") . T.pack . takeBaseName . T.unpack

-- | From Templates.Partials.Buttons: filters button text for name attribute.
buttonName :: Text -> Text
buttonName = T.filter (\c -> isAlphaNum c || isSpace c)

-- | From Templates.Partials.Containers: CSS class for body visibility.
bodyClasses :: Bool -> Text
bodyClasses shown = if shown then "" else "hidden"

-- | From Templates.Partials.Invitebar: feedback text from email validation.
emailText :: Text -> Text
emailText e = "Invitation sent to " <> e

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- | Filename-safe characters (letters, digits, hyphens, underscores)
genFilenameChar :: Gen Char
genFilenameChar =
    Gen.choice
        [ Gen.alphaNum
        , Gen.element ['-', '_']
        ]

-- | Generate a simple filename (no path separators or dots)
genBasename :: Gen String
genBasename = Gen.string (Range.linear 1 30) genFilenameChar

-- | Generate a file path like "/some/path/my-file.png"
genFilePath :: Gen Text
genFilePath = do
    dirs <- Gen.list (Range.linear 0 3) $ Gen.string (Range.linear 1 10) Gen.alphaNum
    base <- genBasename
    ext <- Gen.element [".png", ".jpg", ".svg", ".gif"]
    let path = concatMap (\d -> "/" ++ d) dirs ++ "/" ++ base ++ ext
    pure (T.pack path)

-- | Generate button label text
genButtonLabel :: Gen Text
genButtonLabel = T.pack <$> Gen.string (Range.linear 0 50) Gen.unicode

-- | Generate an email-like text
genEmail :: Gen Text
genEmail = do
    user <- T.pack <$> Gen.string (Range.linear 1 20) Gen.alphaNum
    domain <- T.pack <$> Gen.string (Range.linear 1 15) Gen.alphaNum
    pure (user <> "@" <> domain <> ".com")

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

main :: IO ()
main =
    defaultMain $
        testGroup
            "templates"
            [ mkBasicNameTests
            , buttonNameTests
            , bodyClassesTests
            , emailTextTests
            ]

-- ---------------------------------------------------------------------------
-- mkBasicName tests
-- ---------------------------------------------------------------------------

mkBasicNameTests :: TestTree
mkBasicNameTests =
    testGroup
        "mkBasicName (Image.hs)"
        [ testCase "strips path and extension, replaces dashes" $
            mkBasicName "/assets/profile-photo.png" @?= "profile photo"
        , testCase "simple filename" $
            mkBasicName "hello.jpg" @?= "hello"
        , testCase "multiple dashes" $
            mkBasicName "/a/b/my-cool-image.svg" @?= "my cool image"
        , testCase "no dashes" $
            mkBasicName "/path/to/logo.png" @?= "logo"
        , testCase "underscores preserved" $
            mkBasicName "my_file.png" @?= "my_file"
        , testPropertyNamed
            "dashes become spaces"
            "prop_mkBasicName_dashes"
            prop_mkBasicName_dashes
        , testPropertyNamed
            "result contains no path separators"
            "prop_mkBasicName_no_separators"
            prop_mkBasicName_no_separators
        , testPropertyNamed
            "result contains no file extension"
            "prop_mkBasicName_no_extension"
            prop_mkBasicName_no_extension
        ]

-- | Dashes in the basename become spaces
prop_mkBasicName_dashes :: Property
prop_mkBasicName_dashes = property $ do
    path <- forAll genFilePath
    let result = mkBasicName path
    assert $ not (T.any (== '-') result)

-- | Result never contains path separators
prop_mkBasicName_no_separators :: Property
prop_mkBasicName_no_separators = property $ do
    path <- forAll genFilePath
    let result = mkBasicName path
    assert $ not (T.any (== '/') result)
    assert $ not (T.any (== '\\') result)

-- | Result never contains the dot from a file extension
prop_mkBasicName_no_extension :: Property
prop_mkBasicName_no_extension = property $ do
    base <- T.pack <$> forAll genBasename
    let path = "/dir/" <> base <> ".png"
        result = mkBasicName path
    -- The result should be the basename with dashes replaced
    result === T.replace "-" " " base

-- ---------------------------------------------------------------------------
-- buttonName tests
-- ---------------------------------------------------------------------------

buttonNameTests :: TestTree
buttonNameTests =
    testGroup
        "buttonName (Buttons.hs)"
        [ testCase "keeps alphanumeric and spaces" $
            buttonName "Submit Form!" @?= "Submit Form"
        , testCase "strips special chars" $
            buttonName "Click (here)" @?= "Click here"
        , testCase "empty input" $
            buttonName "" @?= ""
        , testPropertyNamed
            "output contains only alphanumeric and space"
            "prop_buttonName_chars"
            prop_buttonName_chars
        , testPropertyNamed
            "idempotent"
            "prop_buttonName_idempotent"
            prop_buttonName_idempotent
        ]

-- | Output only contains alphanumeric chars and spaces
prop_buttonName_chars :: Property
prop_buttonName_chars = property $ do
    label <- forAll genButtonLabel
    let result = buttonName label
    assert $ T.all (\c -> isAlphaNum c || isSpace c) result

-- | Applying buttonName twice gives the same result
prop_buttonName_idempotent :: Property
prop_buttonName_idempotent = property $ do
    label <- forAll genButtonLabel
    buttonName (buttonName label) === buttonName label

-- ---------------------------------------------------------------------------
-- bodyClasses tests
-- ---------------------------------------------------------------------------

bodyClassesTests :: TestTree
bodyClassesTests =
    testGroup
        "bodyClasses (Containers.hs)"
        [ testCase "shown = True gives empty string" $
            bodyClasses True @?= ""
        , testCase "shown = False gives hidden" $
            bodyClasses False @?= "hidden"
        ]

-- ---------------------------------------------------------------------------
-- emailText tests
-- ---------------------------------------------------------------------------

emailTextTests :: TestTree
emailTextTests =
    testGroup
        "emailText (Invitebar.hs)"
        [ testCase "includes email address" $
            emailText "foo@bar.com" @?= "Invitation sent to foo@bar.com"
        , testPropertyNamed
            "result contains the email"
            "prop_emailText_contains_email"
            prop_emailText_contains_email
        , testPropertyNamed
            "result starts with prefix"
            "prop_emailText_prefix"
            prop_emailText_prefix
        ]

-- | The result always contains the original email
prop_emailText_contains_email :: Property
prop_emailText_contains_email = property $ do
    email <- forAll genEmail
    assert $ email `T.isInfixOf` emailText email

-- | The result always starts with "Invitation sent to "
prop_emailText_prefix :: Property
prop_emailText_prefix = property $ do
    email <- forAll genEmail
    assert $ T.isPrefixOf "Invitation sent to " (emailText email)
