{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Demo (demo) where

import           Control.Lens                             ((.~))
import           Control.Monad                            ((>>))

import           Data.Foldable                            (traverse_)
import qualified Data.Map                                 as Map

import qualified Reflex                                   as R

import           Reflex.Dom.Core                          (MonadWidget, (=:))
import qualified Reflex.Dom.Core                          as RD

import qualified Reflex.Dom.Widget.Input.Datepicker       as D
import           Reflex.Dom.Widget.Input.Datepicker.Types as D

import           Data.Function                            ((&))
import qualified Data.Text                                as Text

import           Data.Time                                (Day)
import qualified Data.Time                                as Time

import           Language.Javascript.JSaddle.WebSockets   (debugOr)
import           Network.Wai.Application.Static           (defaultWebAppSettings,
                                                           staticApp)

-- From Data.Time
-- TimeZone
--     timeZoneMinutes :: Int     -- ^ Number of minutes offset from UTC. Positive means local time will be later in the day than UTC.
--     timeZoneSummerOnly :: Bool -- ^ Is this time zone just persisting for the summer?
--     timeZoneName :: String     -- ^ The name of the zone, typically a three- or four-letter acronym.
--
-- TimeLocale
--     wDays                                    :: [(String, String)] -- ^ full and abbreviated week days, starting with Sunday
--     months                                   :: [(String, String)] -- ^ full and abbreviated months
--     amPm                                     :: (String, String)   -- ^ AM/PM symbols
--     dateTimeFmt, dateFmt, timeFmt, time12Fmt :: String             -- ^ formatting strings
--     knownTimeZones                           :: [TimeZone]         -- ^ time zones known by name

-- Australian TimeLocale
aust :: Time.TimeLocale
aust = Time.defaultTimeLocale
  { Time.dateTimeFmt = Time.iso8601DateFormat (Just "%H:%M:%S")
  , Time.dateFmt     = Time.iso8601DateFormat Nothing
  , Time.knownTimeZones =
    [ Time.TimeZone 600 False "AEST"
    , Time.TimeZone (9 * 60 + 30) False "ACST"
    , Time.TimeZone (8 * 60) False "AWST"
    ]
  }

headSection :: RD.Widget x ()
headSection = do
  RD.el "title" $ RD.text "Date Demo"
  RD.elAttr "meta" ("charset" =: "utf-8") RD.blank
  RD.elAttr "meta" ("name" =: "viewport" <>
                 "content" =: "width=device-width, initial-scale=1, shrink-to-fit=no") RD.blank
  let
    stylesheet s =
      RD.elAttr "link" (Map.fromList [("rel", "stylesheet"), ("href", s)]) RD.blank

  traverse_ stylesheet ["css/reflex-dom-datepicker.css"]

simpleDatePickerUsage
  :: MonadWidget t m
  => Day
  -> m ()
simpleDatePickerUsage today = do
  -- Little page header
  RD.el "h1"
    $ RD.text "Simple Date Widget"

  let
    showDate =
      Text.pack . Time.showGregorian

    cfg = D.simpleDateInputConfig aust
      & D.dateInputConfig_initialValue .~ today

  -- Place the simple date picker on the page. This is a "prebaked" widget that
  -- has a lot of the functionality built into a single component with some flexible styling.
  dateIn <- D.datePickerSimple cfg

  dDaySelect <- R.holdDyn "No Day Clicked" $
    showDate <$> D._dateInput_daySelect dateIn

  -- Show the last day that was clicked from the list of days for the last valid date value
  RD.el "h3" $
    RD.text "Date Selected: " >> RD.dynText dDaySelect

  -- Show the current stored valid day value
  RD.el "h3" $
    RD.text "Date Value: " >> RD.dynText ( showDate <$> D._dateInput_value dateIn )

demo :: IO ()
demo = do
  today <- Time.utctDay <$> Time.getCurrentTime
  debugOr 9999
    (RD.mainWidgetWithHead headSection $ simpleDatePickerUsage today)
    $ staticApp (defaultWebAppSettings "./.")
