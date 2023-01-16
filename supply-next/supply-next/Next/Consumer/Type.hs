module Next.Consumer.Type
  (
    {- * Type aliases -} Consumer, ConsumerPlus,
  )
  where

import Next.Interface (Next, TerminableStream)
import SupplyChain (Job)

{-| A 'Job' whose upstream interface is 'Next' -}
type Consumer action item product =
    Job (Next item) action product

{-| Like 'Consumer', but with a more general upstream interface
    which can be anything in the 'TerminableStream' class

This type is like 'Consumer' except that it has an extra type parameter
representing the upstream interface, hence its name is "consumer /plus/". -}
type ConsumerPlus up action item product =
    TerminableStream item up =>
        Job up action product
