{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Reflex.Dom.Widget.Input.Datepicker.Types where

import           Control.Lens               (Rewrapped, Unwrapped,
                                             makeWrapped, (^.), makeLenses,
                                             _Wrapped)

import           Reflex                     (Dynamic, Event, Reflex)
import           Reflex.Dom                 (MonadWidget, TextInput)

import           Data.Map                   (Map)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified Data.Time                  as Time
import           Data.Time.Calendar         (Day)
import           Data.Time.Format           (TimeLocale)

import qualified GHCJS.DOM.HTMLInputElement as Input

-- This might be a nice idea to pull out
newtype Wrap a t m = Wrap
  { wrapEl :: forall e. MonadWidget t m => m e -> m e
  }

data DayW
data DayListW
data DateW
data ControlW
data DatePickerW
data MonthBtnW
-- or not...

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
makeLenses ''DateInput

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
makeLenses ''DateInputConfig

data DatePickerControls t = DatePickerControls
  { _dateControls_ePrevMonth :: Event t ()
  , _dateControls_textInput  :: TextInput t
  , _dateControls_eNextMonth :: Event t ()
  }
makeLenses ''DatePickerControls

data DatePickerCore t = DatePickerCore
  { _dateCore_parseDate :: Text -> Maybe Day
  , _dateCore_initValue :: Day
  , _dateCore_textInput :: Event t Text
  , _dateCore_setValue  :: Event t Day
  , _dateCore_prevMonth :: Event t ()
  , _dateCore_nextMonth :: Event t ()
  }
makeLenses ''DatePickerCore

fmtDateWith
  :: ( Rewrapped f f
     , Unwrapped f ~ String
     )
  => TimeLocale
  -> f
  -> Day
  -> Text
fmtDateWith tLoc f = Text.pack
  . Time.formatTime tLoc (f ^. _Wrapped)

fmtDate
  :: Reflex t
  => DateInputConfig t
  -> Day
  -> Text
fmtDate cfg = fmtDateWith
  ( cfg ^. dateInputConfig_timelocale )
  ( cfg ^. dateInputConfig_dateFormat )

parseDateWith
  :: ( Rewrapped f f
     , Unwrapped f ~ String
     )
  => TimeLocale
  -> f
  -> Text
  -> Maybe Day
parseDateWith tLoc dFmt =
  Time.parseTimeM True tLoc (dFmt ^. _Wrapped) . Text.unpack

daysInMonth :: Day -> [Day]
daysInMonth day =
  let
    (y, m, _) = Time.toGregorian day
  in
    [ Time.fromGregorian y m d
    | d <- [ 1 .. Time.gregorianMonthLength y m ]
    ]

nextMonth, prevMonth :: Day -> Day
nextMonth = Time.addGregorianMonthsClip 1
prevMonth = Time.addGregorianMonthsClip (-1)
