module Next.Pipe.Type
  (
    {- * Type aliases -} Pipe, PipePlus,
  )
  where

import Next.Interface (Next, TerminableStream)
import SupplyChain (Vendor (..))

{-| A 'Vendor' whose upstream and downstream interfaces are both 'Next' -}
type Pipe action item1 item2 =
    Vendor (Next item1) (Next item2) action

{-| Like 'Pipe', but with a more general upstream interface
    which can be anything in the 'TerminableStream' class

This type is like 'Pipe' except that it has an extra type parameter
representing the upstream interface, hence its name is "pipe /plus/". -}
type PipePlus up action item1 item2 =
    TerminableStream item1 up =>
        Vendor up (Next item2) action
