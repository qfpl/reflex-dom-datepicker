{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
module Reflex.Dom.Widget.Input.Datepicker
  ( datePickerSimple
  , datePickerDateInput
  , simpleDateInputConfig
  ) where

import           Control.Lens                                 (to, (^.))

import           Reflex                                       (Reflex, Event, Dynamic)
import           Reflex.Dom                                   (MonadWidget)

import qualified Reflex                                       as R
import qualified Reflex.Dom                                   as RD

import           Reflex.Dom.Widget.Input.Datepicker.Controls  as RDPControls
import           Reflex.Dom.Widget.Input.Datepicker.Core      as RDPCore
import           Reflex.Dom.Widget.Input.Datepicker.DaySelect as RDPDaySelect
import           Reflex.Dom.Widget.Input.Datepicker.Style     as RDPStyle
import           Reflex.Dom.Widget.Input.Datepicker.Types     as RDPTypes

import           Data.Time                                    (Day, fromGregorian)
import           Data.Time.Format                             (TimeLocale)

simpleDateInputConfig
  :: Reflex t
  => TimeLocale
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

datePickerDateInput
  :: ( Reflex t
     )
  => DatePickerControls t
  -> Event t Day
  -> Dynamic t Day
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

datePickerSimple
  :: MonadWidget t m
  => DateInputConfig t
  -> m (DateInput t)
datePickerSimple dateInpCfg =
  wrapEl RDPStyle.datePickerWrap $ mdo
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

  dateCtrl <- RDPControls.mkDatePickerControls dateInpCfg
    RDPStyle.datePickerControlsWrap
    (R.updated dDayValue)

  eDaySelect <- RDPDaySelect.dayList
    (dateInpCfg ^. dateInputConfig_timelocale)
    (dateInpCfg ^. dateInputConfig_dayFormat)
    (dateInpCfg ^. dateInputConfig_dayAttrs)
    RDPStyle.dayElWrap
    RDPStyle.dayListWrap
    dDaysInMonth

  pure $ datePickerDateInput
    dateCtrl
    eDateSetVal
    dDayValue
