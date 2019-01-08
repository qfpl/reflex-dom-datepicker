{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
-- | Core types for the DatePicker widget
module Reflex.Dom.Widget.Input.Datepicker.Types where

import           Control.Lens               (Rewrapped, Unwrapped, makeLenses,
                                             makeWrapped, (^.), _Wrapped)

import           Reflex                     (Dynamic, Event, Reflex)
import           Reflex.Dom.Core            (MonadWidget, TextInput)

import           Data.Map                   (Map)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text

import qualified Data.Time                  as Time
import           Data.Time.Calendar         (Day)
import           Data.Time.Format           (TimeLocale)

import           GHCJS.DOM.HTMLInputElement (HTMLInputElement)

-- | Simple newtype with a phantom type 'a', to make it easier to specify which
-- wrapper function it contains.
newtype Wrap a t m = Wrap
  { wrapEl :: forall e. MonadWidget t m => m e -> m e
  }

-- Void functions to have a type level identifier for the different wrapper functions
data DayW
-- ^ Individual 'Day' element wrapper
data DayListW
-- ^ Wraps the list of 'Day's in the month
data ControlW
-- ^ Wraps the controls - Month buttons, 'TextInput'
data DatePickerW
-- ^ Wraps the entire Datepicker widget
data MonthBtnW
-- ^ Wraps a single month movement button

-- | Passing raw 'String' values around is not cool, give that puppy a 'newtype'.
newtype DateFormat = DateFormat String
  deriving (Show, Eq)
makeWrapped ''DateFormat

-- | See 'DateFormat'
newtype DayFormat = DayFormat String
  deriving (Show, Eq)
makeWrapped ''DayFormat

-- | Returns a format for ISO8601 Dates:
-- | "%Y-%m-%d" i.e. YYYY-MM-DD
defaultDateFormat :: DateFormat
defaultDateFormat = DateFormat
  $ Time.iso8601DateFormat Nothing

-- | Returns a format for "%d" to display the day
defaultDayFormat :: DayFormat
defaultDayFormat = DayFormat "%d"

-- | This datepicker uses, for better or worse, the Modified Julian `Day` is a
-- standard count of days, with zero being the day 1858-11-17
data DateInput t = DateInput
  { _dateInput_value       :: Dynamic t Day          -- ^ Currently selected 'Day'
  -- Text input box for date selection
  , _dateInput_rawInput    :: Event t Text           -- ^ Raw input of the 'TextInput'
  , _dateInput_keypress    :: Event t Word           -- ^ Key pressed in the 'TextInput'
  , _dateInput_keydown     :: Event t Word           -- ^ Key down in 'TextInput'
  , _dateInput_keyup       :: Event t Word           -- ^ Key up in 'TextInput'
  , _dateInput_hasFocus    :: Dynamic t Bool         -- ^ If the 'TextInput' has focus
  , _dateInput_textElement :: HTMLInputElement -- ^ The 'HTMLInputElement' of the 'TextInput'
  -- Widget mouse/touch actions:
  , _dateInput_daySelect   :: Event t Day            -- ^ Day clicked
  , _dateInput_nextMonth   :: Event t ()             -- ^ Next month button clicked
  , _dateInput_prevMonth   :: Event t ()             -- ^ Prev month button clicked
  }
makeLenses ''DateInput

-- | Configuration for the DatePicker widget, requires setting of 'TimeLocale'
-- and a starting 'Day' value. As well as the labels for the next/previous month
-- buttons. Formats for parsing and display of the 'Day' values, these formats must
-- match what is expected of 'Data.Time.Format'.
--
-- Also required are the CSS attribute maps that will be used for the
-- 'TextInput', 'Day' select @div@s and the next/previous month buttons.
data DateInputConfig t = DateInputConfig
  { _dateInputConfig_initialValue   :: Day                               -- ^ Starting value
  , _dateInputConfig_dateFormat     :: DateFormat                        -- ^ Formatter to be used to check any text input
  , _dateInputConfig_dayFormat      :: DayFormat                         -- ^ Formatter for displaying the days in the month
  , _dateInputConfig_timelocale     :: TimeLocale                        -- ^ This is required for formatting / parsing
  , _dateInputConfig_setValue       :: Event t Day                       -- ^ Fires on selecting or inputting a valid date
  , _dateInputConfig_textInputAttrs :: Dynamic t (Map Text Text)         -- ^ 'TextInput' attributes
  , _dateInputConfig_dayAttrs       :: Dynamic t (Bool -> Map Text Text) -- ^ Attribute function for individual 'Day' values
  , _dateInputConfig_mthBtnAttrs    :: Dynamic t (Map Text Text)         -- ^ month button attributes
  , _dateInputConfig_prevMonthLabel :: Text                              -- ^ Label for Previous month button
  , _dateInputConfig_nextMonthLabel :: Text                              -- ^ Label for Next month button
  }
makeLenses ''DateInputConfig

-- | Provided by the 'mkDatePickerControls' function, contains the 'Event's that are triggered by two buttons, as well as the configuration that is provided by the inner 'TextInput'
data DatePickerControls t = DatePickerControls
  { _dateControls_ePrevMonth :: Event t ()
  , _dateControls_textInput  :: TextInput t
  , _dateControls_eNextMonth :: Event t ()
  }
makeLenses ''DatePickerControls

-- | The configuration required by 'mkDatePickerCore'.
data DatePickerCore t = DatePickerCore
  { _dateCore_parseDate :: Text -> Maybe Day -- ^ 'Day' parsing function
  , _dateCore_initValue :: Day               -- ^ Starting value
  , _dateCore_textInput :: Event t Text      -- ^ 'Text' has been entered.
  , _dateCore_setValue  :: Event t Day       -- ^ External 'Event' to arbitrarily set the 'Day' value of the widget
  , _dateCore_prevMonth :: Event t ()        -- ^ Move to previous month
  , _dateCore_nextMonth :: Event t ()        -- ^ Move to the next month
  }
makeLenses ''DatePickerCore

-- | 'Day' formatter helper function to apply the given wrapped format and present a textual representation.
fmtDateWith
  :: ( Rewrapped f f
     , Unwrapped f ~ String
     )
  => TimeLocale -- ^ This value is taken from the 'DateInputConfig'
  -> f          -- ^ Wrapped format to use with 'formatTime'
  -> Day        -- ^ Input 'Day'
  -> Text       -- ^ Formatted 'Text' output
fmtDateWith tLoc f = Text.pack
  . Time.formatTime tLoc (f ^. _Wrapped)

-- | Format the full date value.
fmtDate
  :: Reflex t
  => DateInputConfig t -- ^ Widget config
  -> Day               -- ^ Input
  -> Text              -- ^ Formatted date output
fmtDate cfg = fmtDateWith
  ( cfg ^. dateInputConfig_timelocale )
  ( cfg ^. dateInputConfig_dateFormat )

-- | Helper to use the given format to see if the input can be parsed into a 'Day'.
parseDateWith
  :: ( Rewrapped f f
     , Unwrapped f ~ String
     )
  => TimeLocale
 -> f          -- ^ Wrapped format to use for validating 'Text' input to a 'Day' value
 -> Text       -- ^ 'Text' input
  -> Maybe Day -- ^ Might be a valid 'Day'
parseDateWith tLoc dFmt =
  Time.parseTimeM True tLoc (dFmt ^. _Wrapped) . Text.unpack

-- | Return a list of @Day@ values to match the month of the given date.
--
-- Uses @gregorianMonthLength@.
daysInMonth :: Day -> [Day]
daysInMonth day =
  let
    (y, m, _) = Time.toGregorian day
  in
    [ Time.fromGregorian y m d
    | d <- [ 1 .. Time.gregorianMonthLength y m ]
    ]

-- | Move to the next/previous month based on the given @Day@.
--
-- This movement will clip the date to the month. So if you have 30th Jan
-- selected and you move forward to February then the @Day@ will be changed to
-- the 28th to ensure a valid @Day@ is selected.
nextMonth, prevMonth :: Day -> Day
nextMonth = Time.addGregorianMonthsClip 1
prevMonth = Time.addGregorianMonthsClip (-1)
