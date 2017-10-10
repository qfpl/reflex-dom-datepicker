{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
module Reflex.Dom.Widget.Input.Datepicker
  ( datePickerSimple
  , simpleDateInputConfig
  , defaultDateFormat
  , defaultDayFormat
  , DateInput          (..)
  , DateInputConfig    (..)
  , Wrap (..)
  ) where

import           Control.Applicative                          (liftA3)
import           Control.Lens                                 (Lens', to, (.~),
                                                               (^.))

import           Reflex                                       (Event, Reflex,
                                                               (<@))
import           Reflex.Dom                                   (MonadWidget,
                                                               (=:))

import qualified Reflex                                       as R
import qualified Reflex.Dom                                   as RD

import           Reflex.Dom.Widget.Input.Datepicker.Controls  as RDPControls
import           Reflex.Dom.Widget.Input.Datepicker.Core      as RDPCore
import           Reflex.Dom.Widget.Input.Datepicker.DaySelect as RDPDaySelect
import           Reflex.Dom.Widget.Input.Datepicker.Style     as RDPStyle
import           Reflex.Dom.Widget.Input.Datepicker.Types     as RDPTypes

import           Data.Function                                ((&))
import           Data.Text                                    (Text)

import           Data.Time                                    (Day)
import qualified Data.Time                                    as Time
import           Data.Time.Format                             (TimeLocale)

simpleDateInputConfig
  :: Reflex t
  => TimeLocale
  -> DateInputConfig t
simpleDateInputConfig tL = DateInputConfig
  (Time.fromGregorian 1970 1 1)
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

datePickerSimple
  :: MonadWidget t m
  => DateInputConfig t
  -> m (DateInput t)
datePickerSimple dateInpCfg =
  wrapEl RDPStyle.datePickerWrap $ mdo
  let
    initialVal  = dateInpCfg ^. dateInputConfig_initialValue
    initialDays = daysInMonth initialVal
    fmtDt       = fmtDate dateInpCfg
    pDate       = parseDateWith
                  (dateInpCfg ^. dateInputConfig_timelocale)
                  (dateInpCfg ^. dateInputConfig_dateFormat)

    eDateSetVal = R.leftmost
      [ dateInpCfg ^. dateInputConfig_setValue
      , eDaySelect
      ]

  (dDayValue, dDaysInMonth) <- RDPCore.mkDatePickerCore $
    RDPTypes.DatePickerCore pDate initialVal
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

  return $ DateInput dDayValue
    (dateCtrl ^. dateControls_textInput . RD.textInput_input)
    (dateCtrl ^. dateControls_textInput . RD.textInput_keypress)
    (dateCtrl ^. dateControls_textInput . RD.textInput_keydown)
    (dateCtrl ^. dateControls_textInput . RD.textInput_keyup)
    (dateCtrl ^. dateControls_textInput . RD.textInput_hasFocus)
    (dateCtrl ^. dateControls_textInput . to RD._textInput_element)
    eDaySelect
    (dateCtrl ^. dateControls_ePrevMonth )
    (dateCtrl ^. dateControls_eNextMonth )
