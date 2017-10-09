{-# LANGUAGE OverloadedStrings     #-}
module Reflex.Dom.Widget.Input.Datepicker.DaySelect
  ( dayList
  , dayDynEl
  ) where

import           Reflex                                      (Dynamic, Event,
                                                              (<@))
import           Reflex.Dom                                  (MonadWidget)

import qualified Reflex                                      as R
import qualified Reflex.Dom                                  as RD

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes

import Data.Text (Text)
import Data.Map (Map)
import Data.Time (TimeLocale)
import           Data.Time.Calendar                          (Day)

dayDynEl
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat
  -> Dynamic t (Map Text Text)
  -> Dynamic t Day
  -> m (Event t Day)
dayDynEl tLoc dFmt dAttrs dDay = do
  (e, _) <- RD.elDynAttr' "div" dAttrs $
    RD.dynText (fmtDateWith tLoc dFmt <$> dDay)

  pure $ R.current dDay <@ RD.domEvent RD.Click e

dayList
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat
  -> Dynamic t (Map Text Text)
  -> DayWrapper t m
  -> DayListWrapper t m
  -> Dynamic t [Day]
  -> m (Event t Day)
dayList tLoc dayFmt dayAttrs dayWrap dayListWrap dDaysInMonth =
  R.switch . R.current . fmap R.leftmost <$>
    wrapDayList dayListWrap
    ( RD.simpleList dDaysInMonth
      ( wrapDay dayWrap . dayDynEl tLoc dayFmt dayAttrs )
    )
