module Reflex.Dom.Widget.Input.Datepicker.Core
  ( datePickerCore
  ) where

import           Reflex                                   (Event, Reflex)
import qualified Reflex                                   as R

import           Data.Text                                (Text)
import           Data.Time                                (TimeLocale)
import           Data.Time.Calendar                       (Day)

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes

datePickerCore
  :: Reflex t
  => TimeLocale
  -> DateFormat
  -> Event t Text -- ^ The text input
  -> Event t Day  -- ^ External 'set value' event
  -> Event t Day  -- ^ previous month event
  -> Event t Day  -- ^ next month event
  -> Event t Day  -- ^ day 'selected/clicked' event
  -> Event t Day  -- ^ one event to find them all and in the darkness, leftmost them
datePickerCore tLoc dateFmt eTextInput eSetValue ePrevMonth eNextMonth eDaySelect =
  R.leftmost [ prevMonth <$> ePrevMonth
             , nextMonth <$> eNextMonth
             , eSetValue
             , eDaySelect
             , R.fmapMaybe (parseDateWith tLoc dateFmt) eTextInput
             ]
