module Next.Producer.State
  (
    {- * State actions -} null, head, push, pop,
  )
  where

import Essentials
import Next.Interface
import Next.Producer.Type

import Control.Monad.Trans.State.Strict (StateT (StateT))
import SupplyChain ((>->), Referral (Referral))

import qualified Control.Monad.Trans.State.Strict as State
import qualified Data.Foldable as Foldable
import qualified Next.Pipe as Pipe
import qualified SupplyChain.Vendor as Vendor

{-| Test whether the state is an empty stream -}
null :: forall action item. Monad action =>
    StateT (Producer action item) action Bool
null = head <&> Foldable.null

{-| Peek at the first item in the stream state -}
head :: forall action item. Monad action =>
    StateT (Producer action item) action (Step item)
head = pop >>= \x -> traverse_ push x $> x

{-| Add an item to the front of the stream state -}
push :: forall up action item. Monad action =>
    item -> StateT (ProducerPlus up action item) action ()
push x = State.modify' (>-> Pipe.cons (pure x))

{- | Take the first item from the stream -}
pop :: forall action item. Monad action =>
    StateT (Producer action item) action (Step item)
pop = StateT \xs ->
    Vendor.run xs next <&> \(Referral s v) -> (s, v)
