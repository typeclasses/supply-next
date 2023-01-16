module Next.Producer.Examples
  (
    {- * Trivialities -} empty, singleton, effect, each,
    {- * Append -} append,
    {- * Unfold -} unfoldJob, unfoldEffect, unfoldPure,
  )
  where

import Essentials
import Next.Interface
import Next.Producer.Type

import SupplyChain (Job, Vendor (..), Referral (..))

import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified SupplyChain.Job as Job

{-| The empty stream -}
empty :: forall up item action. ProducerPlus up action item
empty = go
  where
    go :: Vendor up (Next item) action
    go = Vendor \Next -> pure $ Referral End go

{-| Yields one item, then stops -}
singleton :: forall up item action.
    Job up action item -> ProducerPlus up action item
singleton x = Vendor \Next -> x <&> \y -> Referral (Item y) empty

{-| A single item obtained by performing an effect -}
effect :: forall up action item. action item -> ProducerPlus up action item
effect x = singleton $ Job.perform x

{-| Yields all the items from the given list -}
each :: forall up foldable item action. Foldable foldable =>
    foldable item -> ProducerPlus up action item
each = Foldable.toList >>> unfoldPure (List.uncons >>> maybe End Item)

{-| Yields all the items of the first stream, followed by all the items of the second -}
append :: forall up item action. ProducerPlus up action item
    -> ProducerPlus up action item -> ProducerPlus up action item
append a b = Vendor \r@Next -> handle a r >>= \case
    Referral End _ -> handle b r
    Referral (Item x) a' -> pure $ Referral (Item x) (append a' b)

unfoldJob :: forall state up item action.
    (state -> Job up action (Step (item, state)))
    -> state -> ProducerPlus up action item
unfoldJob f = go
  where
    go :: state -> Vendor up (Next item) action
    go s = Vendor \r@Next -> f s >>= \case
        End -> handle empty r
        Item (x, s') -> pure $ Referral (Item x) (go s')

unfoldEffect :: forall state up item action.
    (state -> action (Step (item, state)))
    -> state -> ProducerPlus up action item
unfoldEffect f = unfoldJob (Job.perform . f)

unfoldPure :: forall state up item action.
    (state -> Step (item, state))
    -> state -> ProducerPlus up action item
unfoldPure f = unfoldJob (pure . f)
