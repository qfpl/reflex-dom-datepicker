{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
-- | A date picker UI component, built using the Reflex FRP framework and
-- Reflex-Dom components.
module Reflex.Dom.Widget.Input.Datepicker
  ( datePickerSimple
  , datePickerWrappedWith
  , datePickerDateInput
  , simpleDateInputConfig
  ) where

import           Control.Lens                                 (to, (^.))

import           Reflex                                       (Dynamic, Event,
                                                               Reflex)
import           Reflex.Dom.Core                              (MonadWidget)

import qualified Reflex                                       as R
import qualified Reflex.Dom.Core                              as RD

import           Reflex.Dom.Widget.Input.Datepicker.Controls  as RDPControls
import           Reflex.Dom.Widget.Input.Datepicker.Core      as RDPCore
import           Reflex.Dom.Widget.Input.Datepicker.DaySelect as RDPDaySelect
import           Reflex.Dom.Widget.Input.Datepicker.Style     as RDPStyle
import           Reflex.Dom.Widget.Input.Datepicker.Types     as RDPTypes

import           Data.Time                                    (Day,
                                                               fromGregorian)
import           Data.Time.Format                             (TimeLocale)

-- | 'DateInputConfig' with some simple defaults and using the default CSS attribute maps from 'Reflex.Dom.Widget.Datepicker.Style'
simpleDateInputConfig
  :: Reflex t
  => TimeLocale -- ^ Input TimeLocale for parsing & formatting.
  -> DateInputConfig t
simpleDateInputConfig tL = DateInputConfig
  (fromGregorian 1970 1 1)
  defaultDateFormat
  defaultDayFormat
  tL
  R.never
  RDPStyle.textInputAttrs
  RDPStyle.dayElAttrs
  RDPStyle.monthButtonAttrs
  -- I know Thufir, I'm sitting with my Text to the door...
  "<<"
  ">>"

-- | Simple Datepicker input if you have created your own 'DatePickerControls' and 'Dynamic' for the 'Day'.
datePickerDateInput
  :: ( Reflex t
     )
  => DatePickerControls t -- ^ Controls record: month buttons and 'TextInput'
  -> Event t Day          -- ^ 'Day' set 'Event', if other components need to set this value at some stage
  -> Dynamic t Day        -- ^ The 'Dynamic' for storing the current 'Day' value
  -> DateInput t
datePickerDateInput ctrl eDaySetValue dDayValue = DateInput
  dDayValue
  (ctrl ^. dateControls_textInput . RD.textInput_input)
  (ctrl ^. dateControls_textInput . RD.textInput_keypress)
  (ctrl ^. dateControls_textInput . RD.textInput_keydown)
  (ctrl ^. dateControls_textInput . RD.textInput_keyup)
  (ctrl ^. dateControls_textInput . RD.textInput_hasFocus)
  (ctrl ^. dateControls_textInput . to RD._textInput_element)
  eDaySetValue
  (ctrl ^. dateControls_ePrevMonth)
  (ctrl ^. dateControls_eNextMonth)

-- | The simplest and most rudamentary Datepicker implementation, wires
-- everything up for you based only on the given configuration. Using the inbuilt
-- default styling and wrapper functions.
datePickerSimple
  :: MonadWidget t m
  => DateInputConfig t
  -> m (DateInput t)
datePickerSimple =
  datePickerWrappedWith
    RDPStyle.datePickerWrap
    RDPStyle.datePickerControlsWrap
    RDPStyle.monthButtonWrap
    RDPStyle.dayElWrap
    RDPStyle.dayListWrap

-- | Slightly more configurable Datepicker, allowing you to provide your own
-- wrapper functions for the various components, along with the configuration.
datePickerWrappedWith
  :: MonadWidget t m
  => Wrap DatePickerW t m -- ^ Wrap the whole widget
  -> Wrap ControlW t m    -- ^ Wrap the controls - 'TextInput' and both buttons
  -> Wrap MonthBtnW t m   -- ^ Wrap the month buttons
  -> Wrap DayW t m        -- ^ Wrap the individual 'Day' elements
  -> Wrap DayListW t m    -- ^ Wrap the list of 'Day' elements
  -> DateInputConfig t    -- ^ Configuration
  -> m (DateInput t)
datePickerWrappedWith wDP wCtrl wBtn wDay wDayList dateInpCfg =
  wrapEl wDP $ mdo
  let
    pDate = parseDateWith
      (dateInpCfg ^. dateInputConfig_timelocale)
      (dateInpCfg ^. dateInputConfig_dateFormat)

    eDateSetVal = R.leftmost
      [ dateInpCfg ^. dateInputConfig_setValue
      , eDaySelect
      ]

  (dDayValue, dDaysInMonth) <- RDPCore.mkDatePickerCore $ RDPTypes.DatePickerCore pDate
    (dateInpCfg ^. dateInputConfig_initialValue)
    (dateCtrl ^. dateControls_textInput . RD.textInput_input)
    eDateSetVal
    (dateCtrl ^. dateControls_ePrevMonth)
    (dateCtrl ^. dateControls_eNextMonth)

  dateCtrl <- RDPControls.mkDatePickerControls dateInpCfg wCtrl wBtn
    (R.updated dDayValue)

  eDaySelect <- RDPDaySelect.mkDaySelectDisplay
    (dateInpCfg ^. dateInputConfig_timelocale)
    (dateInpCfg ^. dateInputConfig_dayFormat)
    wDay
    wDayList
    dDayValue
    dDaysInMonth
    (dateInpCfg ^. dateInputConfig_dayAttrs)

  pure $ datePickerDateInput
    dateCtrl
    eDateSetVal
    dDayValue
