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
  , HasDateInput       (..)
  , DateInputConfig    (..)
  , HasDateInputConfig (..)
  , DayListWrapper     (..)
  , DayWrapper         (..)
  ) where

import           Control.Lens                                 (Lens', to, (.~),
                                                               (^.))

import           Reflex                                       (Event, Reflex,
                                                               (<@))
import           Reflex.Dom                                   (MonadWidget,
                                                               (=:))

import qualified Reflex                                       as R
import qualified Reflex.Dom                                   as RD

import           Reflex.Dom.Widget.Input.Datepicker.Core      as RDPCore
import           Reflex.Dom.Widget.Input.Datepicker.DaySelect as RDPDaySelect
import           Reflex.Dom.Widget.Input.Datepicker.Types     as RDPTypes

import           Data.Function                                ((&))
import           Data.Text                                    (Text)

import qualified Data.Time                                    as Time
import           Data.Time.Format                             (TimeLocale)

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

datePickerSimple
  :: MonadWidget t m
  => DateInputConfig t
  -> DayListWrapper t m
  -> DayWrapper t m
  -> m (DateInput t)
datePickerSimple dateInpCfg dayListWrap dayWrap = mdo
  let
    initialVal  = dateInpCfg ^. dateInputConfig_initialValue
    initialDays = daysInMonth initialVal
    fmtDt       = fmtDate dateInpCfg
    monthBtn    = moveMthBtn dateInpCfg

    eDateUpdate = RDPCore.datePickerCore
      ( dateInpCfg ^. dateInputConfig_timelocale )
      ( dateInpCfg ^. dateInputConfig_dateFormat )
      ( tI ^. RD.textInput_input )
      ( dateInpCfg ^. dateInputConfig_setValue )
      ( R.current dDayValue <@ ePrevMonth )
      ( R.current dDayValue <@ eNextMonth )
      eDaySelect

  dDayValue <-
    R.holdDyn initialVal eDateUpdate

  dDaysInMonth <-
    R.holdDyn initialDays ( daysInMonth <$> eDateUpdate )

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

  eDaySelect <- RDPDaySelect.dayList
    (dateInpCfg ^. dateInputConfig_timelocale)
    (dateInpCfg ^. dateInputConfig_dayFormat)
    (dateInpCfg ^. dateInputConfig_dayAttrs)
    dayWrap
    dayListWrap
    dDaysInMonth

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
