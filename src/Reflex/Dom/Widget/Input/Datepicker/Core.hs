{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecursiveDo           #-}
module Reflex.Dom.Widget.Input.Datepicker.Core
  ( mkDatePickerCore
  ) where

import           Control.Lens                             ((^.))

import           Control.Monad.Fix                        (MonadFix)

import           Reflex                                   (Dynamic,
                                                           MonadHold, Reflex, (<@))
import qualified Reflex                                   as R

import           Data.Time.Calendar                       (Day)

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes

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
