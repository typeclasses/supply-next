module Next.Stream.Type
  (
    {- * Type -} Stream (..),
  )
  where

import Essentials
import Next.Producer.Type

import Control.Monad (ap)
import SupplyChain ((>->))

import qualified Next.Pipe as Pipe
import qualified Next.Producer as Producer

newtype Stream action item =
    Stream{ producer :: Producer action item }

instance Semigroup (Stream action item) where
    a <> b = Stream $ Producer.append (producer a) (producer b)

instance Monoid (Stream action item) where
    mempty = Stream Producer.empty

instance Functor (Stream action) where
    fmap f (Stream xs) = Stream $ xs >-> Pipe.map (\x -> pure (f x))

instance Applicative (Stream action) where
    pure x = Stream $ Producer.singleton (pure x)
    (<*>) = ap

instance Monad (Stream action) where
    (Stream xs) >>= f = Stream $
        xs >-> Pipe.concatMapProducer (\x -> producer (f x))
