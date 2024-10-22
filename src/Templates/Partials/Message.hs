{-# LANGUAGE OverloadedStrings #-}
module Templates.Partials.Message where

import Classh
import Classh.Reflex (dynTextS, textS)
import Classh.Upstream (maxW)
import Common.Constants

import Data.Text (Text)
import Data.Int (Int64)
import Database.Beam.Backend.SQL.BeamExtensions (unSerial)
import qualified Data.Text as T
import Data.Time
import Obelisk.Route
import Reflex.Dom.Core
import Templates.Types

import Data.Map (Map)
import qualified Data.Map as Map
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.Element as Element
import GHCJS.DOM.Types (liftJSM, JSM)

import Common.Route
import Common.Schema
import Common.ChatSchema
import Common.View (Msg(..))
import Common.Types (Email(..))

import Classh
import Control.Applicative (liftA2)

getAttachmentId :: Msg -> Maybe Int64
getAttachmentId msg = unSerial <$> unAttachmentId (_msg_attachment msg)

whichSide :: Reflex t => Dynamic t Bool -> Dynamic t T.Text
whichSide = fmap $ \case
  True -> classhUnsafe [position .~~ (J_End, A_Center)]
  False -> classhUnsafe [position .~~ (J_Start, A_Center)]

isMe :: Applicative m => m Email -> m Msg -> m Bool
isMe = liftA2 (\(Email email) mView' -> email == _msg_handle mView')

message :: ( Template t m
           , DOM.MonadJSM (Client m)
           , DOM.MonadJSM (Performable (Client m))
           , DOM.MonadJSM (Client (Client m))
           , Element.IsElement (RawElement (DomBuilderSpace (Client m)))
           , PostBuild t0 (Client m)
           , Prerender t0 m
           )
  => Dynamic t Email -> Dynamic t Msg -> m ()
message clientEmail mView = elDynClass "div" (whichSide $ isMe clientEmail mView) $ do
  let base = $(classh' [ px .~~ TWSize 8
                       , pb .~~ TWSize 6
                       , mb .~~ TWSize 6
                       , ml .|~ [TWSize 16, TWSize 0]                                
                       , w .~~ TWSize_Fit
                       , maxW .~~ DC_Custom "66%"
                       ])
      shell_1 = $(classh' [p .~~ TWSize 4
                          , br .~~ R_2Xl
                          , bc .~~ hex "2E3A59"
                          , bgColor .~~ hex "2E3A59"
                          , w .~~ TWSize_Auto
                          , custom .~ "drop-shadow-2xl"
                          ])
  elClass "li" base $ do
    divClass shell_1 $ do      
      divClass "flex flex-row items-baseline justify-between pb-4" $ do
        divClass "md:mr-2 pb-2 border-b-2 border-[#F1FCFF]" $ do
          dynTextS $(classh' [text_color .~~ hex "D3D3D3"])  $ _msg_userName <$> mView 
          textS $(classh' [text_color .~~ hex "D3D3D3", text_size .~~ XL]) " >> "
          elClass "span" $(classh' [mr .~ [("md", TWSize 4)]] ) $ do
            dynTextS $(classh' [text_color .~~ hex "D3D3D3"]) (fmap _msg_handle mView)
          dynTextS $(classh' [text_weight .~~ Bold, text_size .~~ XL, text_color .~~ hex "D3D3D3"] ) $ timestamp . _msg_timestamp <$> mView

      let payload = zipDyn (getAttachmentId <$> mView) (_msg_text <$> mView)
      -- It's assumed the backend has already sanitized the input here.
      dyn_ $ ffor payload $ \case
        (Nothing, msg) -> prerender_ blank $ do
          elClass "div" "text-white text-2xl" $ elDynHtml_ "div" (pure msg)
          pure ()
        (Just aId, msg) | T.null msg -> attachment aId
        (Just aId, msg) -> do
          prerender_ blank $ do
            elClass "div" "text-white text-2xl" $ elDynHtml_ "div" (pure msg)
            pure ()
          elClass "hr" "my-4" blank
          attachment aId

attachment :: Template t m => Int64 -> m ()
attachment aId = do
  let url = renderBackendRoute checkedFullRouteEncoder $
        BackendRoute_DownloadAttachment :/ aId
  elAttr "a"
    ( "target" =: "_blank"
      <> "class" =: "text-blue-400"
      <> "href" =: url
      <> "download" =: ""
    ) $
    text ("Attachment " <> T.pack (show aId))

timestamp :: UTCTime -> Text
timestamp = T.pack . formatTime defaultTimeLocale "%R"

elDynHtmlAttr_ :: (Reflex t
                  , DOM.MonadJSM m
                  , DomBuilder t m
                  , PostBuild t m
                  , PerformEvent t m
                  , DOM.MonadJSM (Performable m)
                  , DOM.MonadJSM (Client m)
                  , Element.IsElement (RawElement (DomBuilderSpace m))
                  ) => Text -> Map Text Text -> Dynamic t Text -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtmlAttr_ elementTag attrs html = do
  let cfg = def & initialAttributes .~ Map.mapKeys (AttributeName Nothing) attrs
  (e, _) <- element elementTag cfg $ return ()
  postBuild <- getPostBuild
  performEvent_ $ liftJSM . Element.setInnerHTML (_element_raw e) <$> leftmost [updated html, tag (current html) postBuild]
  return e

elDynHtml_ :: (Reflex t
              , DOM.MonadJSM m
              , DomBuilder t m
              , PostBuild t m
              , PerformEvent t m
              , DOM.MonadJSM (Performable m)
              , DOM.MonadJSM (Client m)
              , Element.IsElement (RawElement (DomBuilderSpace m))
              ) => Text -> Dynamic t Text -> m (Element EventResult (DomBuilderSpace m) t)
elDynHtml_ elementTag = elDynHtmlAttr_ elementTag mempty
