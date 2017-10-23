module Reflex.Dom.Widget.Input.Datepicker.Core
  ( mkDatePickerCore
  ) where

import           Control.Arrow                            ((&&&))
import           Control.Lens                             ((^.))

import           Control.Monad.Fix                        (MonadFix)

import           Reflex                                   (Dynamic, MonadHold,
                                                           Reflex)
import qualified Reflex                                   as R

import           Data.Time.Calendar                       (Day)

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes

-- | Core functionality of the DatePicker widget.
--
-- Takes a structure containing some configuration as well as the input 'Event's
-- from the Controls:
--
-- * Previous month button click
--
-- * Next month button click
--
-- * Text input
--
-- * Set value 'Event'
--
-- The resulting 'Dynamic' contains the 'Day' of the date that is selected or
-- set, and the list of 'Day's that are in the month for the selected 'Day'.
mkDatePickerCore
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     )
  => DatePickerCore t -- ^ Core input 'Event's and 'TextInput'
  -> m (Dynamic t Day, Dynamic t [Day])
mkDatePickerCore dpCore =
  -- Turn our Day in to (Day, [Day])
  fmap (id &&& fmap daysInMonth)
  . R.foldDyn ($) (dpCore ^. dateCore_initValue) $ R.mergeWith (.)
    [ prevMonth <$  _dateCore_prevMonth dpCore
    , nextMonth <$  _dateCore_nextMonth dpCore
    , const     <$> _dateCore_setValue dpCore
    , const     <$> R.fmapMaybe (dpCore ^. dateCore_parseDate) (dpCore ^. dateCore_textInput)
    ]
