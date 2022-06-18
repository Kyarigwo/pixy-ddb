{-# LANGUAGE NoImplicitPrelude #-}

module UtilSpec (spec) where

import Relude hiding(Predicate)
import Pixy.Data hiding (script)
import qualified Pixy.Relational as R
import Pixy
import Test.Hspec
import qualified Data.Set as Set
import qualified Data.Map as Map

spec :: Spec
spec = do
  describe "row operations" $ do
    it "can create a simple row" $ R.row [(#a, 1)] `shouldBe` R.row [(#a, 1)]
    it "can merge two rows" $
      R.row [(#a, 1)] `R.merge` R.row [(#b, 2)]
        `shouldBe` Just (R.row [(#b, 2), (#a, 1)])
    it "can merge overlapping rows" $
      R.row [(#a, 1), (#c, 3)] `R.merge` R.row [(#b, 2), (#c, 3)]
        `shouldBe` Just (R.row [(#b, 2), (#a, 1), (#c, 3)])
    it "can merge unequi overlapping rows" $
      R.row [(#a, 1), (#c, 4)] `R.merge` R.row [(#b, 2), (#c, 3)]
        `shouldBe` Nothing
    it "has unitRow as a identity" $
      R.unitRow `R.merge` R.row [(#a, 1), (#b, 2)]
        `shouldBe` Just (R.row [(#a, 1), (#b, 2)])
    evalWorks

evalWorks :: Spec
evalWorks = do
  describe "Basic Eval" $ do
    it "evals the basic database" $ evalScript script `shouldBe` Right db1

db1 :: R.Database
db1 = R.Database {
  relations = Map.fromList [
    (Predicate {name = "link"},
      R.Relation {tuples = Set.fromList [
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "a'"),
          (Label {name = "to"}, Symbol "b'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "b'"),
          (Label {name = "to"}, Symbol "c'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "c'"),
          (Label {name = "to"}, Symbol "c'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "c'"),
          (Label {name = "to"}, Symbol "d'")]}
      ]}),
    (Predicate {name = "reachable"},
      R.Relation {tuples = Set.fromList [
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "a'"),
          (Label {name = "to"}, Symbol "b'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "a'"),
          (Label {name = "to"}, Symbol "c'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "a'"),
          (Label {name = "to"}, Symbol "d'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "b'"),
          (Label {name = "to"}, Symbol "c'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "b'"),
          (Label {name = "to"}, Symbol "d'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "c'"),
          (Label {name = "to"}, Symbol "c'")]},
        R.Row {values = Map.fromList [
          (Label {name = "from"}, Symbol "c'"),
          (Label {name = "to"}, Symbol "d'")]}]
    })]}

-- definitions


script:: Script
script = do
  let link = mkAtom "link"
  let reachable = mkAtom "reachable"

  fact $ link [#from =: #a',#to =: #b']
  fact $ link [#from =: #b',#to =: #c']
  fact $ link [#from =: #c',#to =: #c']
  fact $ link [#from =: #c',#to =: #d']

  reachable[#from =: #x, #to =: #y] <== link [#from =: #x, #to =: #y]
  reachable[#from =: #x, #to =: #y] <== reachable[#from =: #x, #to =: #z]
                                      /\ link [#from =: #z, #to =: #y]