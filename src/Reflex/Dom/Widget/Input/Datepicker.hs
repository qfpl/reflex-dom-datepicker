{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE RecursiveDo            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Reflex.Dom.Widget.Input.Datepicker
  ( datePicker
  , simpleDateInputConfig
  , defaultDateFormat
  , defaultDayFormat
  , DateInput          (..)
  , HasDateInput       (..)
  , DateInputConfig    (..)
  , HasDateInputConfig (..)
  , DayListWrapper     (..)
  , DayWrapper         (..)
  ) where

import           Control.Lens               (Lens', Rewrapped, Unwrapped,
                                             makeClassy, makeWrapped, to, (.~),
                                             (^.), _1, _2, _Wrapped)

import           Reflex                     (Dynamic, Event, Reflex, (<@))
import           Reflex.Dom                 (MonadWidget, (=:))

import qualified Reflex                     as R
import qualified Reflex.Dom                 as RD

import           Data.Function              ((&))
import           Data.Map                   (Map)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified Data.Time                  as Time
import           Data.Time.Calendar         (Day)
import           Data.Time.Format           (TimeLocale)

import qualified GHCJS.DOM.HTMLInputElement as Input

-- This might be a nice idea to pull out
-- data Day'
-- data Date'
-- newtype Wrap a t m = Wrap
--   { wrapEl :: forall e. MonadWidget t m => m e -> m e
--   }
-- eg
-- type DayWrap = Wrap Day'
-- type DateWrap = Wrap Date'
-- therefore
-- DayWrap !~ DateWrap ? 

newtype DayWrapper t m = DayWrapper
  { wrapDay :: forall a. MonadWidget t m => m a -> m a
  }

newtype DayListWrapper t m = DayListWrapper
  { wrapDayList :: forall a. MonadWidget t m => m a -> m a
  }

newtype DateFormat = DateFormat String
  deriving (Show, Eq)
makeWrapped ''DateFormat

newtype DayFormat = DayFormat String
  deriving (Show, Eq)
makeWrapped ''DayFormat

-- | Returns a format for ISO8601 Dates:
-- | "%Y-%m-%d" i.e. YYYY-MM-DD
defaultDateFormat
  :: DateFormat
defaultDateFormat =
  DateFormat $ Time.iso8601DateFormat Nothing

defaultDayFormat
  :: DayFormat
defaultDayFormat =
  DayFormat "%d"

-- The Modified Julian `Day` is a standard count of
-- days, with zero being the day 1858-11-17
data DateInput t = DateInput
  { _dateInput_value       :: Dynamic t Day
  -- Text input box for date selection
  , _dateInput_rawInput    :: Event t Text
  , _dateInput_keypress    :: Event t Word
  , _dateInput_keydown     :: Event t Word
  , _dateInput_keyup       :: Event t Word
  , _dateInput_hasFocus    :: Dynamic t Bool
  , _dateInput_textElement :: Input.HTMLInputElement
  -- Widget mouse/touch actions:
  , _dateInput_daySelect   :: Event t Day -- ^ Day clicked
  , _dateInput_nextMonth   :: Event t () -- ^ Next month button clicked
  , _dateInput_prevMonth   :: Event t () -- ^ Prev month button clicked
  }
makeClassy ''DateInput

data DateInputConfig t = DateInputConfig
  { _dateInputConfig_initialValue   :: Day         -- ^ Starting value
  , _dateInputConfig_dateFormat     :: DateFormat  -- ^ Formatter to be used to check any text input
  , _dateInputConfig_dayFormat      :: DayFormat   -- ^ Formatter for displaying the days in the month
  , _dateInputConfig_timelocale     :: TimeLocale  -- ^ This is required for formatting / parsing
  , _dateInputConfig_setValue       :: Event t Day -- ^ Fires on selecting or inputting a valid date
  , _dateInputConfig_textInputAttrs :: Dynamic t (Map Text Text)
  , _dateInputConfig_dayAttrs       :: Dynamic t (Map Text Text)
  , _dateInputConfig_mthBtnAttrs    :: Dynamic t (Map Text Text)
  , _dateInputConfig_prevMonthLabel :: Text
  , _dateInputConfig_nextMonthLabel :: Text
  }
makeClassy ''DateInputConfig

simpleDateInputConfig
  :: Reflex t
  => TimeLocale
  -> DateInputConfig t
simpleDateInputConfig tL =
  let
    noAttrs = R.constDyn mempty
    dayAttrs = pure ("class" =: "day-item")
  in
    DateInputConfig
    (Time.fromGregorian 1970 1 1)
    defaultDateFormat
    defaultDayFormat
    tL
    R.never
    noAttrs dayAttrs noAttrs
    "<<" ">>"

fDate
  :: ( Reflex t
     , HasDateInputConfig d t
     , Rewrapped f f
     , Unwrapped f ~ String
     )
  => Lens' d f
  -> d
  -> Day
  -> Text
fDate l cfg = Text.pack . Time.formatTime
  (cfg ^. dateInputConfig_timelocale)
  (cfg ^. l . _Wrapped)

fmtDate, fmtDay
  :: ( Reflex t
     , HasDateInputConfig d t
     )
  => d
  -> Day
  -> Text
fmtDate = fDate dateInputConfig_dateFormat
fmtDay = fDate dateInputConfig_dayFormat

parseDate
  :: ( Reflex t
     , HasDateInputConfig d t
     )
  => d
  -> Text
  -> Maybe Day
parseDate cfg = Time.parseTimeM True
  (cfg ^. dateInputConfig_timelocale)
  (cfg ^. dateInputConfig_dateFormat . _Wrapped)
  . Text.unpack

moveMthBtn
  :: ( MonadWidget t m
     , HasDateInputConfig d t
     )
  => d
  -> Lens' d Text
  -> m (Event t ())
moveMthBtn cfg l = RD.elDynAttr "div"
  (cfg ^. dateInputConfig_mthBtnAttrs)
  (RD.button $ cfg ^. l)

dayDynEl
  :: MonadWidget t m
  => DateInputConfig t
  -> Dynamic t Day
  -> m (Event t Day)
dayDynEl cfg dDay = do
  let dAttrs = cfg ^. dateInputConfig_dayAttrs

  (e, _) <- RD.elDynAttr' "div" dAttrs $
    RD.dynText (fmtDay cfg <$> dDay)

  let
    eDayClicked = RD.domEvent RD.Click e

  pure $ R.current dDay <@ eDayClicked

daysInMonth
  :: Day
  -> [Day]
daysInMonth day =
  let
    gregDay = Time.toGregorian day

    y = gregDay ^. _1
    m = gregDay ^. _2
  in
    [ Time.fromGregorian y m d
    | d <- [ 1 .. Time.gregorianMonthLength y m ]
    ]

nextMonth, prevMonth :: Day -> Day
nextMonth = Time.addGregorianMonthsClip 1
prevMonth = Time.addGregorianMonthsClip (-1)

datePicker
  :: MonadWidget t m
  => DateInputConfig t
  -> DayListWrapper t m
  -> DayWrapper t m
  -> m (DateInput t)
datePicker dateInpCfg dayListWrap dayWrap = mdo
  let
    initialVal  = dateInpCfg ^. dateInputConfig_initialValue
    initialDays = daysInMonth initialVal
    fmtDt       = fmtDate dateInpCfg
    monthBtn    = moveMthBtn dateInpCfg

    eDateUpdate = R.leftmost
      [ prevMonth <$> R.current dDayValue <@ ePrevMonth
      , nextMonth <$> R.current dDayValue <@ eNextMonth
      , dateInpCfg ^. dateInputConfig_setValue
      , eDaySelect
      -- Will not be fired on 'textInput_setValue' Event triggers, only direct input
      , R.fmapMaybe (parseDate dateInpCfg) $ tI ^. RD.textInput_input
      ]

  dDayValue <- R.holdDyn initialVal eDateUpdate
  dDaysInMonth <- R.holdDyn initialDays ( daysInMonth <$> eDateUpdate )

  ePrevMonth <-
    monthBtn dateInputConfig_prevMonthLabel

  tI <- RD.textInput $ RD.def
    -- Set our initial value by formatting the given Day using the given format
    & RD.textInputConfig_initialValue .~
      dateInpCfg ^. dateInputConfig_initialValue . to fmtDt
    -- Pass along the attrs we've been given
    & RD.textInputConfig_attributes .~
      dateInpCfg ^. dateInputConfig_textInputAttrs
    -- Create the update Event by formatting the latest update
    & RD.textInputConfig_setValue .~ fmap fmtDt eDateUpdate

  eNextMonth <-
    monthBtn dateInputConfig_nextMonthLabel

  eDaySelect <- R.switch . R.current . fmap R.leftmost <$>
    wrapDayList dayListWrap
      ( RD.simpleList dDaysInMonth
        ( wrapDay dayWrap . dayDynEl dateInpCfg )
      )

  return $ DateInput
    dDayValue
    (tI ^. RD.textInput_input)
    (tI ^. RD.textInput_keypress)
    (tI ^. RD.textInput_keydown)
    (tI ^. RD.textInput_keyup)
    (tI ^. RD.textInput_hasFocus)
    (RD._textInput_element tI)
    eDaySelect
    ePrevMonth
    eNextMonth
