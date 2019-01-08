{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
-- | Functions for the display and selection of the individual day in the
-- currently selected month.
module Reflex.Dom.Widget.Input.Datepicker.DaySelect
  ( mkDaySelectDisplay
  , dayDynEl
  ) where

import           Reflex                                   (Dynamic, Event, (<@))
import           Reflex.Dom.Core                          (MonadWidget)

import qualified Reflex                                   as R
import qualified Reflex.Dom.Core                          as RD

import qualified Reflex.Dom.Widget.Basic.SelectViews      as SelectView

import           Reflex.Dom.Widget.Input.Datepicker.Types as RDPTypes


import           Data.Map                                 (Map)

import           Data.Text                                (Text)
import           Data.Time                                (TimeLocale)
import           Data.Time.Calendar                       (Day)

-- | This function is responsible for rendering a single 'Day' value in a `div`
-- that provides a 'Click' Event.
dayDynEl
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat                 -- ^ The format used to render this particular 'Day' value
  -> Dynamic t Day             -- ^ The value that will be rendered.
  -> Dynamic t (Map Text Text) -- ^ Attributes for the `div` used to surround this 'Day'
  -> m (Event t Day)
dayDynEl tLoc dFmt dDay dAttrs = do
  (e, _) <- RD.elDynAttr' "div" dAttrs
    $ RD.dynText (fmtDateWith tLoc dFmt <$> dDay)

  pure $ R.current dDay <@ RD.domEvent RD.Click e

-- | Render the list of days in the month provided by the 'Dynamic' value.
--
-- It will wrap the entire list in whatever is provided by the 'DayListW'
-- wrapper function, and the individual 'Day' elements will be wrapped using the
-- 'DayW' function.
--
-- Attributes are generated using the @Dynamic t ( Bool -> Map Text Text )@ with
-- the 'Bool' being 'True' if the day being rendered is the 'Day' that is
-- selected.
mkDaySelectDisplay
  :: MonadWidget t m
  => TimeLocale
  -> DayFormat                         -- ^ Format for 'Data.Time.Format.formatTime' to display an individual 'Day' of the month
  -> Wrap DayW t m                     -- ^ Some 'MonadWidget' wrapper for the individual 'Day' elements
  -> Wrap DayListW t m                 -- ^ Some 'MonadWidget' wrapper for the entire list element
  -> Dynamic t Day                     -- ^ Represents the currently selected 'Day' value
  -> Dynamic t [Day]                   -- ^ The list of 'Day's in this month
  -> Dynamic t (Bool -> Map Text Text) -- ^ Attributes for the individual days. 'Bool' indicates if this 'Day' is the currently selected value
  -> m (Event t Day)
mkDaySelectDisplay tLoc dayFmt dayWrap dayListWrap dDayValue dDaysInMonth dAttributes =
  wrapEl dayListWrap
  ( SelectView.selectViewListDynAttr dDayValue dDaysInMonth dAttributes
    (\dV dA -> wrapEl dayWrap $ dayDynEl tLoc dayFmt dV dA )
  )
