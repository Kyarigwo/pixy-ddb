module Pixy.Relational where

import Relude hiding(Predicate, head, tail, empty, filter)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Lens as Lens
import Control.Lens(Lens', (^.), (%~))
import Pixy.Data
import Fmt((+|), (|+))
import qualified Fmt
import qualified Data.Text as Text

{-
## Rows

-}
newtype Row = Row {
  values:: Map Label Value
} deriving (Show, Eq, Ord)
{-
### Creation

-}
row :: [(Label, Value)] -> Row
row = Map.fromList >>> Row

unitRow :: Row
unitRow = Row Map.empty
{-
### Conversion

-}
rowToGroundedAtom :: Predicate -> Row -> GroundedAtom
rowToGroundedAtom p r = Atom p (r ^. values)

{-
### Access

-}
values :: Lens' Row (Map Label Value)
values = Lens.lens (.values) s
  where s r m = r{ values = m}

type instance Lens.IxValue Row = Value
type instance Lens.Index Row = Label

instance Lens.Ixed Row where

instance Lens.At Row where
  at l = values . Lens.at l

{-
## Merge Implementation

-}
rowToAscList :: Row -> [(Label, Value)]
rowToAscList = Lens.view values >>> Map.toAscList

rowFromAscList :: [(Label, Value)] -> Row
rowFromAscList = Map.fromAscList >>> Row

mergeLists :: [(Label, Value)] -> [(Label, Value)] -> Maybe [(Label, Value)]
mergeLists [] ys = Just ys
mergeLists xs [] = Just xs
mergeLists xs@((xk, xv) : xs')  ys@((yk, yv) : ys')
  | xk < yk = ((xk, xv):) <$> mergeLists xs' ys
  | xk > yk = ((yk, yv):) <$> mergeLists xs ys'
  | xk == yk && xv == yv = ((yk, yv):) <$> mergeLists xs' ys'
  | otherwise = Nothing

merge :: Row -> Row -> Maybe Row
merge r r' =
  mergeLists (rowToAscList r) (rowToAscList r')
    & fmap rowFromAscList

{-
## Relations

-}
newtype Relation = Relation {
  tuples:: Set Row
} deriving (Show, Eq)

{-
### Creation

-}
relation :: [Row] -> Relation
relation = Set.fromList >>> Relation

empty :: Relation
empty = Relation Set.empty

unit :: Relation
unit = relation [unitRow]

{-
### Conversion

-}
relationToGroundedAtoms :: Predicate -> Relation -> [GroundedAtom]
relationToGroundedAtoms p r =
  r ^. tuples
  & Set.toList
  & fmap (rowToGroundedAtom p)

{-
### Access

-}
tuples :: Lens' Relation (Set Row)
tuples = Lens.lens (.tuples) s
  where s r t = r{ tuples = t}

{-
### Union

-}
union :: Relation -> Relation -> Relation
union r r' = Relation $ Set.union (r ^. tuples) (r' ^. tuples)

{-
Union is a commutative, idemopotent monoid with the empty
relation being the identity.

-}
newtype Unioned = Unioned {getUnioned :: Relation}

instance Semigroup Unioned where
  Unioned r <> Unioned r' = Unioned $ union r r'

instance Monoid Unioned where
  mempty = Unioned empty

{-
### Join


-}
joinRelation :: Relation -> Relation -> Relation
joinRelation rl rl' =
  [merge r r' |
          r <- Set.toList $ rl ^. tuples,
          r' <- Set.toList $ rl' ^. tuples]
    & catMaybes
    & relation

{-
Join is a commutative, idemopotent monoid with the unit
relation being the identity.

-}
newtype Joined = Joined { joined :: Relation}

getJoined :: Joined -> Relation
getJoined = (.joined)

instance Semigroup Joined where
  Joined r <> Joined r' = Joined $ joinRelation r r'

instance Monoid Joined where
  mempty = Joined unit

joined :: Foldable f => f Relation -> Relation
joined = getJoined . foldMap Joined


{-
### Filter and remap

-}

data FilterElement =
  EqValue Label Value
  | EqLabel Label Label

data Filter = Filter [FilterElement]

filterElementToFunction:: FilterElement -> (Row -> All)
filterElementToFunction = \case
  EqValue l v -> \r -> All (r ^. Lens.at l == Just v)
  EqLabel l l' -> \r -> All (r ^. Lens.at l == r ^. Lens.at l')

filter:: Filter -> Relation -> Relation
filter (Filter filterElements) =
  runFilter $ getAll . foldMap filterElementToFunction filterElements
  where runFilter p r = Relation $ Set.filter p (r ^. tuples)

data RewriteMap =
  RewriteValue Label Value
  | RewriteLookupLabel Label Variable
  | RewriteLookupVariable Variable Label

rewriteLookupLabel:: Label -> Variable -> RewriteMap
rewriteLookupLabel = RewriteLookupLabel

rewriteLookupVariable:: Variable -> Label -> RewriteMap
rewriteLookupVariable = RewriteLookupVariable

data Rewrite =
  Rewrite [RewriteMap]

rewriteToFunction:: Rewrite -> Row -> Row
rewriteToFunction (Rewrite rewrites) =
  foldMap rewriteMapToFunction rewrites >>> row

rewriteMapToFunction:: RewriteMap -> Row -> [(Label, Value)]
rewriteMapToFunction = \case
  RewriteValue l v -> \_ -> [(l, v)]
  RewriteLookupLabel l v -> \r -> case r ^. Lens.at l of
    Just v' -> [(variableToLabel v, v')]
    Nothing -> error "Not Found"
  RewriteLookupVariable v l -> \r -> case r ^. Lens.at (variableToLabel v) of
    Just v' -> [(l, v')]
    Nothing -> error "Not Function"

runRewrite :: Rewrite -> Relation -> Relation
runRewrite = rewriteToFunction >>> remap

remap :: (Row -> Row) -> Relation -> Relation
remap f r = Relation $ Set.map f (r ^. tuples)

{-
## Database

-}
newtype Database = Database {
  relations:: Map Predicate Relation
} deriving (Show, Eq)

emptyDatabase :: Database
emptyDatabase = Database Map.empty

relations :: Lens' Database (Map Predicate Relation)
relations = Lens.lens (.relations) s
  where s d m = d{ relations = m}

databaseToGroundedAtoms :: Database -> [GroundedAtom]
databaseToGroundedAtoms db =
  db ^. relations
  & Map.toList
  & concatMap (uncurry relationToGroundedAtoms)

type instance Lens.IxValue Database = Relation
type instance Lens.Index Database = Predicate

instance Lens.Ixed Database where

instance Lens.At Database where
  at p = relations . Lens.at p

rel :: Predicate -> Lens' Database Relation
rel p = Lens.at p . Lens.non empty

instance Semigroup Database where
  d <> d' =
    Database $ Map.unionWith union
        (d ^. relations) (d' ^. relations)

instance Monoid Database where
  mempty = emptyDatabase

unionRelation :: Database -> Predicate -> Relation -> Database
unionRelation db predicate update =
  db & rel predicate %~ union update

{- [markdown]
## Formatting instances

-}

instance Fmt.Buildable Database where
  build db =
    databaseToGroundedAtoms db
    & fmap (\c -> "  "+|c|+"\n")
    & Text.concat
    & Fmt.build