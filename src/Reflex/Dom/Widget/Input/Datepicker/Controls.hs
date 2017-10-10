{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Reflex.Dom.Widget.Input.Datepicker.Controls
  ( moveMonthBtn
  , mkDatePickerControls
  ) where

import           Control.Lens                             (Lens', to, (.~),
                                                           (^.))

import           Data.Function                            ((&))
import           Data.Text                                (Text)
import           Data.Time                                (Day)

import           Reflex                                   (Event)
import           Reflex.Dom                               (MonadWidget, button,
                                                           def, elDynAttr,
                                                           textInput,
                                                           textInputConfig_attributes,
                                                           textInputConfig_initialValue,
                                                           textInputConfig_setValue)

import           Reflex.Dom.Widget.Input.Datepicker.Types (ControlW,
                                                           DateInputConfig,
                                                           DatePickerControls (..),
                                                           dateInputConfig_initialValue,
                                                           dateInputConfig_prevMonthLabel,
                                                           dateInputConfig_nextMonthLabel,
                                                           dateInputConfig_textInputAttrs,
                                                           dateInputConfig_mthBtnAttrs,
                                                           Wrap (..), fmtDate)

moveMonthBtn
  :: ( MonadWidget t m
     )
  => DateInputConfig t
  -> Lens' ( DateInputConfig t ) Text
  -> m (Event t ())
moveMonthBtn cfg l = elDynAttr "div"
  (cfg ^. dateInputConfig_mthBtnAttrs)
  (cfg ^. l . to button)

mkDatePickerControls
  :: MonadWidget t m
  => DateInputConfig t
  -> Wrap ControlW t m
  -> Event t Day
  -> m (DatePickerControls t)
mkDatePickerControls dCfg cWrap eDateUpdate = wrapEl cWrap $ do
  ePrevMonth <- moveMonthBtn dCfg dateInputConfig_prevMonthLabel

  tI <- textInput $ def
      -- Set our initial value by formatting the given Day using the given format
      & textInputConfig_initialValue .~ dCfg ^. dateInputConfig_initialValue . to ( fmtDate dCfg )
      -- Pass along the attrs we've been given
      & textInputConfig_attributes .~ dCfg ^. dateInputConfig_textInputAttrs
      -- Create the update Event by formatting the latest update
      & textInputConfig_setValue .~ fmap ( fmtDate dCfg ) eDateUpdate

  eNextMonth <- moveMonthBtn dCfg dateInputConfig_nextMonthLabel

  return $ DatePickerControls ePrevMonth tI eNextMonth

