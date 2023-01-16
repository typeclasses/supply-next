module Next.Interface.Class
  (
    {- * Class -} TerminableStream (..),
    {- * Utilities -} next,
  )
  where

import Next.Interface.Type

{-| An interface for which 'Next' is one of possibly many supported requests -}
class TerminableStream item interface | interface -> item where

    {-| Lift a 'Next' request into a larger interface -}
    liftNext :: Next item result -> interface result

instance TerminableStream item (Next item) where
    liftNext x = x

{-| Like v'Next', but polymorphic -}
next :: TerminableStream item interface => interface (Step item)
next = liftNext Next
