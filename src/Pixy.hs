module Pixy where

import Relude hiding (head, tail, Predicate)
import Control.Lens((^.))
import qualified Data.Map as Map
import Pixy.Data
import Pixy.Relational (Database, Relation)
import Pixy.Relational qualified as R

{- [markdown]
# Naive Evaluation
-}

evalScript :: Script -> Either [Text] Database
evalScript =
  compile >=> (fixedPointSingleStep R.emptyDatabase) >>> pure

fixedPointSingleStep :: Database -> Compiled -> Database
fixedPointSingleStep db c =
  let db' = singleStep c db
  in if db' == db then db else fixedPointSingleStep db' c

singleStep :: Compiled -> Database -> Database
singleStep c =
  foldMap singleStepClause c.clauses

singleStepClause :: CompiledClause -> Database -> Database
singleStepClause cc db =
  cc.antecedents
    & fmap (antecdentRelation db)
    & R.joined
    & R.runRewrite (cc.consequent).remap
    & R.unionRelation db (cc.consequent.predicate)


antecdentRelation :: Database -> Antecedent -> Relation
antecdentRelation db antecedent =
  db ^. R.rel antecedent.predicate
    & R.filter antecedent.filter
    & R.runRewrite antecedent.remap

compile:: Script -> Either [Text] Compiled
compile s =
  case validScript s of
    Just errs -> Left errs
    Nothing -> Right $ Compiled { clauses = compileClause <$> s.clauses}

newtype Compiled = Compiled {
  clauses:: [CompiledClause]
}

data CompiledClause = CompiledClause {
  antecedents:: [Antecedent],
  consequent:: Consequent
}

data Antecedent = Antecedent {
  predicate:: Predicate,
  filter:: R.Filter,
  remap:: R.Rewrite
}

data Consequent = Consequent {
  predicate:: Predicate,
  remap:: R.Rewrite
}


compileAntecedent:: Atom -> Antecedent
compileAntecedent Atom{..} =
  let (rfilter, rmap) =
        runState (traverse filterAndMap (Map.toList tail)) Map.empty
      f = antecedentFilter rfilter
      r = antecedentRemap rmap
  in Antecedent{predicate = head, filter = f, remap = r}

compileClause:: Clause -> CompiledClause
compileClause Clause {..} =
  let antecedents = fmap compileAntecedent antecedent
      consequents = compileConsequent consequent
  in CompiledClause{antecedents, consequent = consequents}

compileConsequent:: Atom -> Consequent
compileConsequent a =
  let r = consequentRemap a
  in Consequent {predicate = a.head, remap = r}

filterAndMap ::
  (Label, Term) ->
  State (Map Variable Label) (Maybe R.FilterElement)
filterAndMap (label, term) = get >>= filterAndMap' term
  where
    filterAndMap' (Value value) _ =
      valueEqual value
    filterAndMap' (Var variable) (Map.lookup variable -> Just label') =
      varEqual label'
    filterAndMap' (Var variable) (Map.lookup variable -> Nothing) =
      insertValue variable
    filterAndMap' _ _ = error ""

    valueEqual value =
      (R.EqValue label value)
        & Just
        & pure
    varEqual label' =
      (R.EqLabel label label')
        & Just
        & pure
    insertValue variable =
      modify' (Map.insert variable label)
        >> pure Nothing

antecedentFilter :: [Maybe R.FilterElement] -> R.Filter
antecedentFilter = catMaybes >>> R.Filter

antecedentRemap :: Map Variable Label -> R.Rewrite
antecedentRemap mp =
  Map.toList mp
    & fmap singleAntecedentRemap
    & R.Rewrite

singleAntecedentRemap :: (Variable, Label) -> R.RewriteMap
singleAntecedentRemap (v, l) = R.rewriteLookupLabel l v

consequentRemap :: Atom -> R.Rewrite
consequentRemap Atom {..} =
  Map.toList tail
    & fmap singleConsequentRemap
    & R.Rewrite

singleConsequentRemap :: (Label, Term) -> R.RewriteMap
singleConsequentRemap (l, t) =
  case t of
    Value v -> R.RewriteValue l v
    Var v -> R.rewriteLookupVariable v l
