module Next
  (
    {- * Next -} {- $next -} Next (..), Step (..),
            TerminableStream (..), next,
    {- * Producer -} {- $producer -} Producer, ProducerPlus,
            empty, singleton, effect, each, append,
            null, head, pop, push,
            unfoldPure, unfoldEffect, unfoldJob,
    {- * Pipe -} {- $pipe -} Pipe, PipePlus,
            cons, map, concat, takeWhile, dropWhile, group,
            intersperse, beforeEach, concatMapJob, concatMapProducer,
    {- * Consumer -} {- $consumer -} Consumer, ConsumerPlus,
          foldPure, foldEffect, foldJob, toList, run,
    {- * Stream -} {- $stream -} Stream (Stream),
  )
  where

import Next.Interface (Step(..), Next(..), TerminableStream (..), next)
import Next.Consumer (Consumer, ConsumerPlus, foldPure, foldEffect, foldJob, toList, run)
import Next.Producer (Producer, ProducerPlus, empty, singleton, effect, each, append, unfoldPure, unfoldEffect, unfoldJob, null, head, pop, push)
import Next.Pipe (Pipe, PipePlus,
    map, concatMapJob, concat, takeWhile, dropWhile,
    group, cons, intersperse, beforeEach, concatMapProducer)
import Next.Stream (Stream (Stream))

{- $next
See "Next.Interface" -}

{- $producer
See "Next.Producer" -}

{- $pipe
See "Next.Pipe" -}

{- $consumer
See "Next.Consumer" -}

{- $stream
See "Next.Stream" -}
