module Next.Producer
  (
    {- * Types -} {- $types -} Producer, ProducerPlus,
    {- * Examples -} {- $examples -}
            empty, singleton, effect, each, append,
            unfoldPure, unfoldEffect, unfoldJob,
    {- * State actions -} {- $state -}
            null, head, pop, push,
  )
  where

import Next.Producer.Type
import Next.Producer.Examples
import Next.Producer.State

{- $types
See "Next.Producer.Type" -}

{- $examples
See "Next.Producer.Examples" -}

{- $state
See "Next.Producer.State" -}
