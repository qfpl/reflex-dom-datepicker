{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Reflex.Dom.Widget.Input.Datepicker.Controls
  ( moveMonthBtn
  , mkDatePickerControls
  ) where

import           Control.Applicative                      (liftA3)
import           Control.Lens                             (Lens', to, (.~),
                                                           (^.))

import           Data.Function                            ((&))
import           Data.Text                                (Text)
import           Data.Time                                (Day)

import           Reflex                                   (Event)
import           Reflex.Dom                               (MonadWidget, def,
                                                           textInput,
                                                           elDynAttr',
                                                           text,
                                                           domEvent,
                                                           textInputConfig_attributes,
                                                           textInputConfig_initialValue,
                                                           textInputConfig_setValue)

import qualified Reflex.Dom                               as RD

import           Reflex.Dom.Widget.Input.Datepicker.Types (ControlW,
                                                           DateInputConfig,
                                                           DatePickerControls (..),
                                                           MonthBtnW, Wrap (..),
                                                           dateInputConfig_initialValue,
                                                           dateInputConfig_mthBtnAttrs,
                                                           dateInputConfig_nextMonthLabel,
                                                           dateInputConfig_prevMonthLabel,
                                                           dateInputConfig_textInputAttrs,
                                                           fmtDate)

-- | Create a button with a label from the 'DateInputConfig' via the given 'Lens''
-- The button will be wrapped using the function from the 'MonthBtnW' wrapper.
moveMonthBtn
  :: MonadWidget t m
  => DateInputConfig t                -- ^ DatePicker configuration
  -> Wrap MonthBtnW t m               -- ^ 'MonadWidget' wrapper function
  -> Lens' ( DateInputConfig t ) Text -- ^ Lens for getting the label from the 'DateInputConfig'
  -> m (Event t ())
moveMonthBtn cfg bWrap l =
  let attrs = cfg ^. dateInputConfig_mthBtnAttrs
      lbl = text (cfg ^. l)
      toEvt = return . domEvent RD.Click . fst
  in
    wrapEl bWrap $
      elDynAttr' "button" attrs lbl >>= toEvt

-- | Display the controls for the DatePicker widget:
--
-- Displyed in order:
--
-- (1) Previous month button
--
-- 2. Text input
--
-- 3. Next month button
--
-- Each of the month movement buttons are wrapped using the given 'MonthBtnW'
-- function, and the entire set of controls is wrapped with the 'ControlW'
-- wrapper function.
--
-- The 'DatePickerControls' is a record containing the various 'Event's and the
-- inner configuration of the 'TextInput'.
mkDatePickerControls
  :: MonadWidget t m
  => DateInputConfig t  -- ^ Widget configuration, containing 'Day' formats and set value 'Event's
  -> Wrap ControlW t m  -- ^ Wrapper function for the set of controls
  -> Wrap MonthBtnW t m -- ^ Inner wrapper function for the individual month buttons
  -> Event t Day        -- ^ External 'Day' update 'Event', such as a 'Day' element being clicked.
  -> m (DatePickerControls t)
mkDatePickerControls dCfg cWrap bWrap eDateUpdate =
  -- Wrap up the whole bunch in an widget of some kind and package
  -- up our control data structure with the Events and TextInput
  wrapEl cWrap $ liftA3 DatePickerControls
    -- Move to previous month
    ( moveMonthBtn dCfg bWrap dateInputConfig_prevMonthLabel )
    -- The text input for typey typey
    ( textInput $ def
      -- Set our initial value by formatting the given Day using the given format
      & textInputConfig_initialValue .~ dCfg ^. dateInputConfig_initialValue . to ( fmtDate dCfg )
      -- Pass along the attrs we've been given
      & textInputConfig_attributes .~ dCfg ^. dateInputConfig_textInputAttrs
      -- Create the update Event by formatting the latest update
      & textInputConfig_setValue .~ fmap ( fmtDate dCfg ) eDateUpdate
    )
    -- Move to next month
    ( moveMonthBtn dCfg bWrap dateInputConfig_nextMonthLabel )
