{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Main where

import           Control.Lens                             ((.~))
import           Control.Monad                            ((>>))

import qualified Reflex                                   as R
import qualified Reflex.Dom                               as RD

import qualified Reflex.Dom.Widget.Input.Datepicker       as D
import           Reflex.Dom.Widget.Input.Datepicker.Types as D

import           Data.Function                            ((&))

import qualified Data.Text                                as Text

import qualified Data.Time                                as Time

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
  , Time.dateFmt = Time.iso8601DateFormat Nothing
  , Time.knownTimeZones =
    [ Time.TimeZone 600 False "AEST"
    , Time.TimeZone (9 * 60 + 30) False "ACST"
    , Time.TimeZone (8 * 60) False "AWST"
    ]
  }

simpleDatePickerUsage
  :: RD.MonadWidget t m
  => m ()
simpleDatePickerUsage = do
  -- Little page header
  RD.el "h1" $ RD.text "Simple Date Widget"

  let
    showDate =
      Text.pack . Time.showGregorian

    cfg = D.simpleDateInputConfig aust
      & D.dateInputConfig_initialValue .~ Time.fromGregorian 2017 2 3

  -- Place the simple date picker on the page. This is a "prebaked" widget that
  -- has a lot of the functionality built into a single component with some flexible styling.
  dateIn <- D.datePickerSimple cfg

  dDaySelect <- R.holdDyn "No Day Clicked" $
    showDate <$> D._dateInput_daySelect dateIn

  dDate <- R.holdDyn "No Day Value" $
    showDate <$> ( R.updated $ D._dateInput_value dateIn )

  -- Show the last day that was clicked from the list of days for the last valid date value
  RD.el "h3" $
    RD.text "Date Selected: " >> RD.dynText dDaySelect

  -- Show the current stored valid day value
  RD.el "h3" $
    RD.text "Date Value: " >> RD.dynText dDate

main :: IO ()
main = do
  RD.mainWidget simpleDatePickerUsage
