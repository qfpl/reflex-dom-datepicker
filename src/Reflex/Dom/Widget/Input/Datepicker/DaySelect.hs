{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
module Reflex.Dom.Widget.Input.Datepicker.DaySelect
  ( mkDaySelectDisplay
  , dayDynEl
  ) where

import           Reflex                                   (Dynamic, Event, (<@))
import           Reflex.Dom                               (MonadWidget)

import qualified Reflex                                   as R
import qualified Reflex.Dom                               as RD

import qualified Reflex.Dom.Widget.Basic.SelectViews      as SelectView

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes


import           Data.Map                                 (Map)

import           Data.Text                                (Text)
import           Data.Time                                (TimeLocale)
import           Data.Time.Calendar                       (Day)

dayDynEl
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat
  -> Dynamic t Day
  -> Dynamic t (Map Text Text)
  -> m (Event t Day)
dayDynEl tLoc dFmt dDay dAttrs = do
  (e, _) <- RD.elDynAttr' "div" dAttrs
    $ RD.dynText (fmtDateWith tLoc dFmt <$> dDay)

  pure $ R.current dDay <@ RD.domEvent RD.Click e

mkDaySelectDisplay
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat
  -> Wrap DayW t m
  -> Wrap DayListW t m
  -> Dynamic t Day
  -> Dynamic t [Day]
  -> Dynamic t (Bool -> Map Text Text)
  -> m (Event t Day)
mkDaySelectDisplay tLoc dayFmt dayWrap dayListWrap dDayValue dDaysInMonth dAttributes =
  wrapEl dayListWrap
  ( SelectView.selectViewListDynAttr dDayValue dDaysInMonth dAttributes
    (\dV dA -> wrapEl dayWrap $ dayDynEl tLoc dayFmt dV dA )
  )

-- mkDaySelectDisplay
--   :: MonadWidget t m
--   => TimeLocale
--   -> DayFormat
--   -> Dynamic t (Map Text Text)
--   -> Wrap DayW t m
--   -> Wrap DayListW t m
--   -> Dynamic t [Day]
--   -> m (Event t Day)
-- mkDaySelectDisplay tLoc dayFmt dayAttrs dayWrap dayListWrap dDaysInMonth =
--   R.switch . R.current . fmap R.leftmost <$>
--     wrapEl dayListWrap
--     ( RD.simpleList dDaysInMonth
--       ( wrapEl dayWrap
--         . dayDynEl tLoc dayFmt dayAttrs
--       )
--     )
