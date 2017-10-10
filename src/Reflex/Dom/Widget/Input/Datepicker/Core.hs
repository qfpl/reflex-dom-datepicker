{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
module Reflex.Dom.Widget.Input.Datepicker.Core
  ( mkDatePickerCore
  ) where

import           Control.Lens                             ((^.))

import           Control.Monad.Fix                        (MonadFix)

import           Reflex                                   (Dynamic, Event,
                                                           MonadHold, Reflex, (<@))
import qualified Reflex                                   as R

import           Data.Text                                (Text)
import           Data.Time                                (TimeLocale)
import           Data.Time.Calendar                       (Day)

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes

-- datePickerCore
--   :: Reflex t
--   => TimeLocale
--   -> DateFormat
--   -> Event t Text -- ^ The text input
--   -> Event t Day  -- ^ External 'set value' event
--   -> Event t Day  -- ^ previous month event
--   -> Event t Day  -- ^ next month event
--   -> Event t Day  -- ^ day 'selected/clicked' event
--   -> Event t Day  -- ^ one event to find them all and in the darkness, leftmost them
-- datePickerCore tLoc dateFmt eTextInput eSetValue ePrevMonth eNextMonth eDaySelect =
--   R.leftmost [ prevMonth <$> ePrevMonth
--              , nextMonth <$> eNextMonth
--              , eSetValue
--              , eDaySelect
--              , R.fmapMaybe (parseDateWith tLoc dateFmt) eTextInput
--              ]

mkDatePickerCore
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     )
  => DatePickerCore t
  -> m (Dynamic t Day, Dynamic t [Day])
mkDatePickerCore dpCore = mdo
  let
    eDayUpdate = R.leftmost
      [ prevMonth <$> R.current dDays <@ _dateCore_prevMonth dpCore
      , nextMonth <$> R.current dDays <@ _dateCore_nextMonth dpCore
      , _dateCore_setValue dpCore
      , R.fmapMaybe (dpCore ^. dateCore_parseDate) $ dpCore ^. dateCore_textInput
      ]

  dDays <- R.holdDyn ( dpCore ^. dateCore_initValue ) eDayUpdate

  pure ( dDays
       , daysInMonth <$> dDays
       )
