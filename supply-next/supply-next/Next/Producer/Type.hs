module Next.Producer.Type
  (
    {- * Type aliases -} Producer, ProducerPlus,
  )
  where

import Essentials
import Next.Interface.Type
import SupplyChain.Vendor

{-| A 'Vendor' whose upstream interface is nothing and whose
    downstream interface is 'Next' -}
type Producer action item =
    Vendor (Const Void) (Next item) action

{-| A 'Vendor' whose downstream interface is 'Next'

This type is like 'Producer' except that it has an extra type parameter
representing the upstream interface, hence its name is "producer /plus/". -}
type ProducerPlus up action item =
    Vendor up (Next item) action
