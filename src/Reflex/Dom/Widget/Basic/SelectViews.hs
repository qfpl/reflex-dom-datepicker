{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
module Reflex.Dom.Widget.Basic.SelectViews
  ( selectViewListWithKeyDynAttr
  , selectViewListWithKeyDynAttr_
  , selectViewListDynAttr
  ) where

import           Control.Monad.Fix (MonadFix)

import           Reflex            (Dynamic, Event)
import           Reflex.Dom        (DomBuilder, MonadHold, PostBuild)

import qualified Reflex            as R
import qualified Reflex.Dom        as RD

import           Data.Map          (Map)
import qualified Data.Map          as Map

import           Data.Text         (Text)

selectViewListDynAttr
  :: forall t m v a.
     ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Eq v
     , Ord v
     )
  => Dynamic t v   -- Current selection
  -> Dynamic t [v] -- Dynamic list of values
  -> Dynamic t (Bool -> Map Text Text) -- Dynamic attributes for the inner element
  -- Function to create a widget for a given key from Dynamic value and Dynamic attributes
  -> (Dynamic t v -> Dynamic t (Map Text Text) -> m (Event t a))
  -- Event that fires when any child's return Event fires.
  -> m (Event t a)
selectViewListDynAttr selection vals attrs mkChild = do
  -- For good performance, this value must be shared across all children
  let selectionDemux = R.demux selection
      valueMap       = Map.fromList . zip [(1::Int)..]

  selectChild <- RD.list ( valueMap <$> vals ) $ \dV -> do
    let selected = dV >>= R.demuxed selectionDemux
    mkChild dV (attrs <*> selected)

  return . R.switchPromptlyDyn $ R.leftmost . Map.elems <$> selectChild

-- Create a dynamically-changing set of widgets with dynamic attributes
-- function, one of which is selected at a time
selectViewListWithKeyDynAttr
  :: forall t m k v a.
     ( DomBuilder t m
     , Ord k
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t k   -- Current selection
  -> Dynamic t (Map k v) -- Dynamic value list
  -> Dynamic t (Bool -> Map Text Text) -- Dynamic attributes for the inner element
  -- Function to create a widget for a given key from Dynamic value and Dynamic
  -- Bool indicating if this widget is currently selected
  -> (k -> Dynamic t v -> Dynamic t (Map Text Text) -> m (Event t a))
  -- Event that fires when any child's return Event fires. Contains key of an
  -- arbitrary firing widget.
  -> m (Event t (k,a))
selectViewListWithKeyDynAttr selection vals attrs mkChild = do
  -- For good performance, this value must be shared across all children
  let selectionDemux = R.demux selection

  selectChild <- RD.listWithKey vals $ \k v -> do
    let selected = R.demuxed selectionDemux k
    selectSelf <- mkChild k v (attrs <*> selected)
    return $ fmap ((,) k) selectSelf

  return . R.switchPromptlyDyn $ R.leftmost . Map.elems <$> selectChild

selectViewListWithKeyDynAttr_
  :: forall t m k v a.
     ( DomBuilder t m
     , Ord k
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t k   -- Current selection
  -> Dynamic t (Map k v) -- Dynamic value list
  -> Dynamic t (Bool -> Map Text Text) -- Dynamic attributes for the inner element
  -- Function to create a widget for a given key from Dynamic value and Dynamic
  -- Bool indicating if this widget is currently selected
  -> (k -> Dynamic t v -> Dynamic t (Map Text Text) -> m (Event t a))
  -- Event that fires when any child's return Event fires. Contains key of an
  -- arbitrary firing widget.
  -> m (Event t k)
selectViewListWithKeyDynAttr_ selection vals attrs mkChild =
  fmap fst <$> selectViewListWithKeyDynAttr selection vals attrs mkChild

