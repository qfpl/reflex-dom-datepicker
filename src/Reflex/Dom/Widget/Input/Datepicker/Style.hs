{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
module Reflex.Dom.Widget.Input.Datepicker.Style
  ( monthButtonAttrs
  , monthButtonWrap
  , dayElAttrs
  , textInputAttrs
  , dayListWrap
  , dayElWrap
  , datePickerWrap
  , datePickerControlsWrap
  ) where

import           Reflex                                       (Dynamic, Reflex)
import           Reflex.Dom                                   (MonadWidget,
                                                               divClass, (=:))

import           Data.Map                                     (Map)
import           Data.Text                                    (Text)

import           Reflex.Dom.Widget.Input.Datepicker.Types     (ControlW,
                                                               MonthBtnW,
                                                               DatePickerW,
                                                               DayListW, DayW,
                                                               Wrap (..))

monthButtonAttrs, dayElAttrs, textInputAttrs :: Reflex t => Dynamic t (Map Text Text)
monthButtonAttrs = pure ("class" =: "month-button")
dayElAttrs       = pure ("class" =: "day-item")
textInputAttrs   = pure ("class" =: "date-text-input")

wrapDiv
  :: MonadWidget t m
  => Text
  -> Wrap a t m
wrapDiv t = Wrap $ divClass t

dayListWrap :: MonadWidget t m => Wrap DayListW t m
dayListWrap = wrapDiv "day-list-wrap contents-floated"

dayElWrap :: MonadWidget t m => Wrap DayW t m
dayElWrap = wrapDiv "day-wrap"

datePickerWrap :: MonadWidget t m => Wrap DatePickerW t m
datePickerWrap = wrapDiv "date-picker-wrap"

datePickerControlsWrap :: MonadWidget t m => Wrap ControlW t m
datePickerControlsWrap = wrapDiv "date-picker-controls contents-floated"

monthButtonWrap :: MonadWidget t m => Wrap MonthBtnW t m
monthButtonWrap = wrapDiv "month-button"
