{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- | Some wrapper \<div\> functions that come preloaded with styles that can be
-- combined with the built-in CSS file for some functional ableit spartan styling.
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

import           Reflex                                   (Dynamic, Reflex)
import           Reflex.Dom.Core                          (MonadWidget,
                                                           divClass, (=:))

import           Data.Map                                 (Map)
import           Data.Text                                (Text)

import           Data.Semigroup                           ((<>))

import           Reflex.Dom.Widget.Input.Datepicker.Types (ControlW,
                                                           DatePickerW,
                                                           DayListW, DayW,
                                                           MonthBtnW, Wrap (..))


monthButtonAttrs, textInputAttrs :: Reflex t => Dynamic t (Map Text Text)

-- | Default attributes for the month movement button: div class="month-button"
monthButtonAttrs = pure ("class" =: "month-button")

-- | Attributes for the 'TextInput': div class="date-text-input"
textInputAttrs   = pure ("class" =: "date-text-input")

-- | Default attribute function for the individual @Day@ elements, provides a function
-- 'Bool -> Map Text Text'
--
-- * True -> class="day-item active"
--
-- * False -> class="day-item"
dayElAttrs :: Reflex t => Dynamic t (Bool -> Map Text Text)
dayElAttrs = pure
  (\selected ->
      "class" =: ( "day-item" <> if selected then " active" else "" )
  )

wrapDiv
  :: MonadWidget t m
  => Text
  -> Wrap a t m
wrapDiv t = Wrap $ divClass t

-- | Wrapper: div class="day-list-wrap contents-floated"
dayListWrap :: MonadWidget t m => Wrap DayListW t m
dayListWrap = wrapDiv "day-list-wrap contents-floated"

-- | Wrapper: div class="day-wrap"
dayElWrap :: MonadWidget t m => Wrap DayW t m
dayElWrap = wrapDiv "day-wrap"

-- | Wrapper: div class="date-picker-wrap"
datePickerWrap :: MonadWidget t m => Wrap DatePickerW t m
datePickerWrap = wrapDiv "date-picker-wrap"

-- | Wrapper: div class="date-picker-controls contents-floated"
datePickerControlsWrap :: MonadWidget t m => Wrap ControlW t m
datePickerControlsWrap = wrapDiv "date-picker-controls contents-floated"

-- | Wrapper: div class="month-button"
monthButtonWrap :: MonadWidget t m => Wrap MonthBtnW t m
monthButtonWrap = wrapDiv "month-button"
