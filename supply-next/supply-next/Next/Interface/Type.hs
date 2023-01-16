module Next.Interface.Type
  (
    {- * Type aliases -} Next (..), Step (..),
  )
  where

import Essentials

{-| A basic dynamic interface for a possibly-finite stream

Once 'End' is returned from a v'Next' request, it is expected that the stream
will thenceforth return 'End' and perform no side effects in response to any
subsequent 'Next' requests. -}
data Next item result =
    (result ~ Step item) => Next
        -- ^ Request the next item from the stream

{-| The result obtained from a v'Next' request -}
data Step item =
    Item item -- ^ An item obtained from the stream
  | End -- ^ Indicates that the stream has ended and there are no more items
    deriving stock (Functor, Foldable, Traversable, Eq, Ord, Show)
