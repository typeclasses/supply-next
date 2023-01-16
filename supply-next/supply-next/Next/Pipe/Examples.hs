module Next.Pipe.Examples
  (
    {- * Triviality -} id,
    {- * Predicates -} takeWhile, dropWhile,
    {- * Insertion -} cons, intersperse, beforeEach,
    {- * Mapping -} map,
    {- * Concatenation -} concat,
    {- * Concat + map -} concatMapJob, concatMapProducer,
    {- * Grouping -} group,
  )
  where

import Essentials hiding (id)

import Next.Interface (Next (..), Step (..), TerminableStream (..), next)
import Next.Pipe.Type (Pipe, PipePlus)
import Next.Producer.Examples (append, empty)
import Next.Producer.Type (Producer, ProducerPlus)
import Integer (Positive)
import Prelude ((+))
import SupplyChain (Job, Vendor (..), Referral (..))

import qualified SupplyChain.Alter as Alter
import qualified SupplyChain.Job as Job
import qualified SupplyChain.Vendor as Vendor

{-| Apply a function to each item in the stream -}
map :: forall item1 item2 action up.
    (item1 -> Job up action item2)
        -- ^ For each input item, this job produces an output item
    -> PipePlus up action item1 item2
map f = go
  where
    go = Vendor \Next -> Job.order next >>= \case
        End -> pure $ Referral End empty
        Item a -> f a <&> \b -> Referral (Item b) go

{-| Applies the function to each result obtained from upstream,
    and yields each result from the list to the downstream -}
concatMapJob :: forall item1 item2 action up.
    (item1 -> Job up action [item2])
        -- ^ For each input item, this job produces any number of output items
    -> PipePlus up action item1 item2
concatMapJob f = Vendor \Next -> go []
  where
    go :: TerminableStream item1 up => [item2] -> Job up action
        (Referral up (Next item2) action (Step item2))
    go = \case
        b : bs -> pure $ Referral (Item b) $ Vendor \Next -> go bs
        [] -> Job.order next >>= \case
            End -> pure $ Referral End empty
            Item a -> f a >>= go

{-| Flattens a stream of lists -}
concat :: forall item action up. PipePlus up action [item] item
concat = concatMapJob pure

{-| Yields the longest prefix matching the predicate and discards the rest -}
takeWhile :: forall item action up.
    (item -> Job up action Bool)
        -- ^ True if this is the sort of thing we'd like to keep
    -> PipePlus up action item item
takeWhile ok = go
  where
    go = Vendor \r@Next -> Job.order (liftNext r) >>= \case
        Item x -> ok x <&> \case
            True -> Referral (Item x) go
            False -> Referral End empty
        _ -> pure $ Referral End empty

{-| Discards the longest prefix matching the predicate and yields the rest -}
dropWhile :: forall item action up.
    (item -> Job up action Bool)
        -- ^ True if this is the sort of thing we'd like to get rid of
    -> PipePlus up action item item
dropWhile bad = Vendor \r@Next ->
    let
        go :: Job up action (Referral up (Next item) action (Step item))
        go = Job.order (liftNext r) >>= \case
          Item x -> bad x >>= \case
              True -> go
              False -> pure $ Referral (Item x) id
          _ -> pure $ Referral End empty
    in
        go

{- | Does nothing at all -}
id :: forall up item action. PipePlus up action item item
id = Vendor.map liftNext

{-| Removes consecutive duplicate items, and yields each item along
    with the size of the repetition

For example, @\"Hrmm..."@ groups into
@[(1, \'H'), (1, \'r'), (2, \'m'), (3, \'.')]@ -}
group :: forall up item action. Eq item => PipePlus up action item (Positive, item)
group =
    Vendor \Next -> Job.order next >>= \case
        End -> pure $ Referral End empty
        Item x -> start x

  where
    start :: TerminableStream item up => item
        -> Job up action (Referral up (Next (Positive, item)) action (Step (Positive, item)))
    start = go 1

    go :: TerminableStream item up => Positive -> item
        -> Job up action (Referral up (Next (Positive, item)) action (Step (Positive, item)))
    go n x = Job.order next >>= \case
        End -> pure $ Referral (Item (n, x)) empty
        Item y ->
          if y == x
          then go (n + 1) x
          else pure $ Referral (Item (n, x)) $ Vendor \Next -> start y

{-| Add one item to the beginning of a stream -}
cons :: forall item action up.
    Job up action item
        -- ^ This job produces an item to add to the front of the list
    -> PipePlus up action item item
cons head = Vendor \Next -> head <&> \x -> Referral (Item x) id

{-| Add an item between each pair of items of a stream

The length of the stream is modified as @\\case{ 0 -> 0; n -> (2 * n) - 1 }@. -}
intersperse :: forall item action up.
    Job up action item
        -- ^ This job generates items that will be inserted in between
        --   the items of the original list
    -> PipePlus up action item item
intersperse i = Vendor \r@Next -> Job.order (liftNext r) <&> \case
    End -> Referral End empty
    Item x -> Referral (Item x) (beforeEach i)

{-| Add an item before each item in a stream

The length of the stream is doubled. -}
beforeEach :: forall item action up.
    Job up action item
        -- ^ This job generates items that will be inserted before each
        --   of the items of the original list
    -> PipePlus up action item item
beforeEach i = Vendor \r@Next -> Job.order (liftNext r) >>= \case
    End -> pure $ Referral End empty
    Item x -> i <&> \i' -> Referral (Item i') $ Vendor \Next ->
        pure $ Referral (Item x) (beforeEach i)

-- | Like 'concatMapJob', but the function gives a 'Producer' instead of a 'Job'
concatMapProducer :: forall item1 item2 action.
    (item1 -> Producer action item2)
        -- ^ For each item from the input list, this vendor generates
        --   any number of actions to yield in the resulting list
    -> Pipe action item1 item2
concatMapProducer f = go
  where
    go = Vendor \Next -> Job.order next >>= \case
        End -> pure $ Referral End empty
        Item x -> handle (append v go) Next
          where
            v = (f x :: ProducerPlus (Const Void) action item2)
                & Alter.vendor' (Alter.request' $ \z -> case z of {})
