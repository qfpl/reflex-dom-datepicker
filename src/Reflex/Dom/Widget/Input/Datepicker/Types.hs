{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE TypeFamilies           #-}
module Reflex.Dom.Widget.Input.Datepicker.Types
  ( DayWrapper (..)
  , DayListWrapper (..)
  , DateInput (..)
  , DateInputConfig (..)
  , HasDateInput (..)
  , HasDateInputConfig (..)
  , DayFormat (..)
  , DateFormat (..)
  , defaultDateFormat
  , defaultDayFormat
  , fmtDateWith
  , fmtDate
  , fmtDay
  , parseDateWith
  , daysInMonth
  , nextMonth
  , prevMonth
  ) where

import           Control.Lens (Lens', Rewrapped, Unwrapped, makeClassy,
                               makeWrapped, (^.), _Wrapped)

import           Reflex       (Dynamic, Event, Reflex)
import           Reflex.Dom   (MonadWidget)

import           Data.Map                                    (Map)
import           Data.Text                                   (Text)
import qualified Data.Text                                   as Text

import qualified Data.Time                                   as Time
import           Data.Time.Calendar                          (Day)
import           Data.Time.Format                            (TimeLocale)

import qualified GHCJS.DOM.HTMLInputElement                  as Input

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

fmtDate, fmtDay
  :: ( Reflex t
     , HasDateInputConfig d t
     )
  => d
  -> Day
  -> Text
fmtDate cfg = fmtDateWith
  ( cfg ^. dateInputConfig_timelocale )
  ( cfg ^. dateInputConfig_dateFormat )

fmtDay cfg = fmtDateWith
  ( cfg ^. dateInputConfig_timelocale )
  ( cfg ^. dateInputConfig_dayFormat )

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

daysInMonth
  :: Day
  -> [Day]
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
