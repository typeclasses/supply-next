module Next.Pipe
  (
    {- * Types -} {- $types -} Pipe, PipePlus,
    {- * Examples -} {- $examples -} cons, map, concat,
        takeWhile, dropWhile, group,
        intersperse, beforeEach, id,
        concatMapJob, concatMapProducer,
  )
  where

import Next.Pipe.Type
import Next.Pipe.Examples

{- $types
See "Next.Pipe.Type" -}

{- $examples
See "Next.Pipe.Examples" -}
