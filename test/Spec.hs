module Main where

import Test.Hspec

import Data.Set (Set)
import qualified Data.Set as Set

import Interface
import PlanTypes

s :: Ord a => [a] -> Set a
s = Set.fromList

main :: IO ()
main = hspec $ do
  describe "Interface" $ do
    it "can ground predicates" predicateGrounding
    it "can ground actions" actionGrounding
  describe "PlanningGraph" $ do
    it "can expand graph" $ do shouldBe 1 2
    it "can extract plan" $ do shouldBe 1 2
    it "can solve planning problem" $ do shouldBe 1 2

predicateGrounding :: Expectation
predicateGrounding = do
  let (a,b,c) = ("a","b","c")
  createFactSet (s ["a","b","c"]) (s [predicate "At" ["x","y"]])
    `shouldBe`
    s (do (a,b) <- [(a,a),(a,b),(a,c),(b,a),(b,b),(b,c),(c,a),(c,b),(c,c)]
          let act = "At(" ++ a ++ "," ++ b ++ ")"
          [prop act, neg $ prop act]
      )

actionGrounding :: Expectation
actionGrounding = do
  let props = s $ do x <- ["a","b"]
                     let t = "At(" ++ x ++ ")"
                     [prop t, neg $ prop t]
  let goAction =
        paramAction "Go"
        [predicate "At" ["x"], neg $ predicate "At" ["y"]]
        [predicate "At" ["y"], neg $ predicate "At" ["x"]]
  createActionSet props (s ["a", "b"]) (s [goAction])
    `shouldBe` (s $ [action "Go(a,b)"
                      (s [prop "At(a)", neg $ prop "At(b)"])
                      (s [prop "At(b)", neg $ prop "At(a)"]),
                     action "Go(b,a)"
                      (s [prop "At(b)", neg $ prop "At(a)"])
                      (s [prop "At(a)", neg $ prop "At(b)"])] ++
                     do x <- ["a","b"]
                        let tag = "At(" ++ x ++ ")"
                        p <- [prop tag, neg $ prop tag]
                        let atag = "[" ++ show p ++ "]"
                        return $ action atag (s [p]) (s [p])
               )
