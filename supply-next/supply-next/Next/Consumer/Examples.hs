module Next.Consumer.Examples
  (
     {- * Trivialities -} toList, run,
     {- * General folding -} foldPure, foldEffect, foldJob,
  )
  where

import Essentials
import Fold.Effectful.Type

import Fold (Fold)
import Next.Consumer.Type (ConsumerPlus)
import Next.Interface (Step (Item, End), next)
import SupplyChain (Job)

import qualified SupplyChain.Job as Job
import qualified Fold.Effectful
import qualified Fold.Pure

{-| Run the stream completely, collecting results using a
    fold that operates in the 'Job' context

See "Fold.Effectful" -}
foldJob :: forall item product action up.
    EffectfulFold (Job up action) item product
    -> ConsumerPlus up action item product
foldJob EffectfulFold{ initial, step, extract } =
    initial >>= go >>= extract
  where
    go b = Job.order next >>= \case
        End -> pure b
        Item a -> step b a >>= go

{-| Run the stream completely, collecting results using an
    effectful fold

See "Fold.Effectful" -}
foldEffect :: Monad action =>
    EffectfulFold action item product
    -> ConsumerPlus up action item product
foldEffect f = foldJob (Fold.Effectful.hoist Job.perform f)

{-| Run the stream completely, collecting results using a
    pure fold

See "Fold.Pure" -}
foldPure :: Monad action =>
    Fold item product
    -> ConsumerPlus up action item product
foldPure f = foldJob (Fold.Effectful.fold f)

{-| Consumes all items and returns them as a list -}
toList :: forall up action item. Monad action =>
    ConsumerPlus up action item [item]
toList = foldPure Fold.Pure.list

{-| Like 'toList', but discards the results -}
run :: forall up action item. Monad action =>
    ConsumerPlus up action item ()
run = foldPure (pure ())
