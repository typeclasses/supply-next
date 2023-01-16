module Main (main) where

import Essentials

import Control.Monad (replicateM)
import Control.Monad.Trans.Writer.CPS (execWriter, tell, runWriter, Writer)
import Data.Char (Char)
import Data.Function (on)
import Data.Monoid (Sum (..))
import Data.Sequence (Seq)
import Data.String (String)
import Integer (Positive)
import Next (each, ProducerPlus, Step (..), Stream (Stream), next)
import Prelude (Integer, Int, IO, (+), show)
import SupplyChain ((>->), (>-), Job, order)
import Test.Hspec (hspec, describe, it, shouldBe)

import qualified Data.Char as Char
import qualified Data.Sequence as Seq
import qualified Fold.Effectful as Fold
import qualified Next
import qualified Next.Stream as Stream
import qualified SupplyChain.Job as Job
import qualified Data.List as List

main :: IO ()
main = hspec $ describe "Next" do

    describe "Producer" do

        it "each" do
            let job = Next.each "abc" >- replicateM 4 (order next)
            Job.eval job `shouldBe` [Item 'a', Item 'b', Item 'c', End]

    describe "Pipe" do

        let write :: Next.ConsumerPlus up (Writer (Seq item)) item ()
            write = Next.foldEffect (Fold.effect (Seq.singleton >>> tell))

        describe "concat" do
            let takeN n input = Next.each input >-> Next.concat >- replicateM n (order next)
            it "1" do
                let job = takeN 5 ["a", "bc", "def", "ghij"]
                Job.eval job `shouldBe` [Item 'a', Item 'b', Item 'c', Item 'd', Item 'e']
            it "2" do
                let job = takeN 5 ["a", "bc"]
                Job.eval job `shouldBe` [Item 'a', Item 'b', Item 'c', End, End]

        describe "group" do
            let input = "Hrmm..."; grouped = [(1, 'H'), (1, 'r'), (2, 'm'), (3, '.')]
            it "pure" do
                let job = Next.each "Hrmm..." >-> Next.group >- Next.toList
                Job.eval job `shouldBe` grouped
            it "effectful" do
                let job = each input >-> Next.group >- write
                execWriter (Job.run job) `shouldBe` Seq.fromList grouped
            it "empty" do
                let job = Next.empty >-> Next.group >- Next.toList
                shouldBe @[(Positive, Char)] (Job.eval job) []

        it "takeWhile" do
            let input = "PORKchOp"; capitalPrefix = "PORK"
                job = each input >-> Next.takeWhile (pure . Char.isUpper) >- write
            execWriter (Job.run job) `shouldBe` Seq.fromList capitalPrefix

        describe "intersperse" do
            let xs, ys :: [Integer]; xs = [1, 2, 3]; ys = [1, 0, 2, 0, 3]

            it "intersperses items" do
                let job = each xs >-> Next.intersperse separator >- Next.toList
                    separator = pure 0
                runIdentity (Job.run job) `shouldBe` ys

            it "also intersperses effects" do
                let job = each xs >-> Next.intersperse separator >- Next.toList
                    separator = 0 <$ Job.perform (tell (Sum (1 :: Integer)))
                runWriter (Job.run job) `shouldBe` (ys, Sum 2)

    describe "Consumer" do

        let write :: Int -> Job up (Writer String) ()
            write x = Job.perform $ tell $ show x

            input = [2, 3, 4] :: [Int]

            producer :: ProducerPlus up (Writer String) Int
            producer = each input >-> Next.map \x -> write x $> x

        it "combines values and effects from the list" do
            let f = Fold.sum; job = producer >- Next.foldEffect f
            runWriter (Job.run job) `shouldBe` (List.sum input, "234")

        it "also incorporates effects from the fold" do
            let f = Fold.sum <* Fold.effect (\n -> replicateM n $ tell "-")
                job = producer >- Next.foldEffect f
            runWriter (Job.run job) `shouldBe` (List.sum input, "2--3---4----")

    describe "Stream" do

        let write :: forall a. a -> Stream (Writer (Seq a)) ()
            write = Stream . Next.effect . tell . Seq.singleton

        it "is a monad" do
            let a, b :: [Integer]; a = [10, 20, 30]; b = [1, 2, 3]
                stream = ((+) <$> Stream (each a) <*> Stream (each b)) >>= write
                job = Stream.producer stream >- Next.run
            execWriter (Job.run job) `shouldBe`
                Seq.fromList [11, 12, 13, 21, 22, 23, 31, 32, 33]

        describe "(<>)" do
            let a, b :: [Integer]; a = [10, 20, 30]; b = [1, 2, 3]
            it "pure" do
                let (===) = shouldBe `on` \s -> do
                        let job = Stream.producer s >- Next.toList
                        runIdentity $ Job.run job
                (Stream (each a) <> Stream (each b)) === Stream (each (a <> b))
            it "effectful" do
                let stream = (Stream (each a) <> Stream (each b)) >>= write
                    job = Stream.producer stream >- Next.run
                shouldBe @(Seq Integer) (execWriter $ Job.run job)
                    (Seq.fromList (a <> b))
