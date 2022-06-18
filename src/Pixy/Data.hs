{- [markdown]
# Datatypes for Pixy

-}
module Pixy.Data where

import Relude hiding (Predicate, head, tail)

import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Fmt
import Fmt((+|), (|+))
import qualified Data.Text as Text

{- [markdown]
## Basic types

-}
newtype Predicate = Predicate {
  name:: Text
} deriving (Show, Eq, Ord)

newtype Label = Label {
  name:: Text
} deriving (Show, Eq, Ord)

newtype Variable = Variable {
  name:: Text
}deriving (Show, Eq, Ord)

variableToLabel :: Variable -> Label
variableToLabel Variable{..} = Label name

data Value =
  String Text
  | Int Int
  | Symbol Text
  deriving(Show, Eq, Ord)

{- [markdown]
## Atoms

Atoms represent basic logical claims.  If the tail of an atom contains terms,
which are both value and varibles it represents a possible claim, while if
only values, it is a fact, which we call a grounded atom.
-}
data Atom' t = Atom {
  head:: Predicate,
  tail:: Map Label t
} deriving(Show, Eq, Ord, Functor, Foldable, Traversable)

data Term =
  Var Variable
  | Value Value

type Atom = Atom' Term

type GroundedAtom = Atom' Value

{- [markdown]

## Clauses
-}
data Clause' a = Clause {
  consequent:: a,
  antecedent:: [a]
} deriving(Show, Eq, Ord, Functor, Foldable, Traversable)

type Clause = Clause' Atom

atomVariables:: Atom -> Set Variable
atomVariables = foldMap $ \case
  Value _ -> Set.empty
  Var v -> Set.singleton v

clauseValidVariables:: Clause -> Maybe [Text]
clauseValidVariables c@Clause{consequent, antecedent} =
  let leftOvers = atomVariables consequent
                    `Set.difference`
                  foldMap atomVariables antecedent
  in if null leftOvers
      then Nothing
      else Just [
        "Clause: "+|c|+
        " unmatched variables in head: "+|toList leftOvers|+""
      ]

data Script' c a = Script {
  clauses:: [c],
  extra:: a
} deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Bifunctor Script' where
  bimap fab fcd Script {clauses, extra}
    = Script {clauses = fmap fab clauses, extra = fcd extra}

instance Bifoldable Script' where
  bifoldMap fam fbm Script {clauses, extra} =
    foldMap fam clauses <> fbm extra

type Script = Script' Clause ()

type Signatures = Map Predicate (Set Label)

atomSignature:: Atom -> Signatures
atomSignature Atom{..} =
  Map.singleton head (Map.keysSet tail)

clauseSignatures:: Clause -> Signatures
clauseSignatures = foldMap atomSignature

scriptSignatures:: Script -> Signatures
scriptSignatures  Script{..} = foldMap clauseSignatures clauses

confirmMatchingAtomSignature:: Signatures -> Atom -> Maybe [Text]
confirmMatchingAtomSignature sig Atom{..} =
  case Map.lookup head sig of
    Nothing -> Just ["Unknown predicate"]
    Just sig' -> if Map.keysSet tail == sig'
                  then Nothing
                  else Just[
                    "Mismatch in Atom Signatures"
              ]

confirmMatchingClauseSignature:: Signatures -> Clause -> Maybe [Text]
confirmMatchingClauseSignature signatures =
  foldMap (confirmMatchingAtomSignature signatures)

scriptValidPredicates:: Script -> Maybe [Text]
scriptValidPredicates s =
  let
    checkSignatures = confirmMatchingClauseSignature (scriptSignatures s)
  in bifoldMap checkSignatures (const mempty) s

validScript :: Script -> Maybe [Text]
validScript =
  bifoldMap clauseValidVariables (const mempty) <> scriptValidPredicates


infixr 5 =:

(=:) :: Label -> t -> Map Label t
(=:) = Map.singleton

mkAtom :: Text -> [Map Label t] -> Atom' t
mkAtom name terms = Atom {
  head = Predicate {
    name = name
  },
  tail = fold terms
}

instance KnownSymbol l => IsLabel l Label where
  fromLabel = Label {
    name = Text.pack (symbolVal (Proxy:: Proxy l))
  }

instance KnownSymbol l => IsLabel l Value where
  fromLabel =
    Symbol $ Text.pack (symbolVal (Proxy:: Proxy l))

instance Num Value where

  (+) (Int l) (Int r) = Int (l + r)
  (+) _ _ = Int 0

  (*) (Int l) (Int r) = Int (l * r)
  (*) _ _ = Int 1

  abs (Int n) = Int (abs n)
  abs _ = Int 0

  signum (Int n) = Int (signum n)
  signum _ = Int 0

  fromInteger = fromInteger >>> Int

  negate (Int n) = Int (negate n)
  negate _ = Int 0


instance IsString Value where
  fromString = Text.pack >>> String

instance KnownSymbol l => IsLabel l Term where
  fromLabel =
    let s = Text.pack $ symbolVal (Proxy :: Proxy l)
    in if "\'" `Text.isSuffixOf` s
          then Value (Symbol (Text.dropEnd 1 s))
          else Var (Variable s)

fact:: GroundedAtom -> Script
fact a = Script {
  clauses = [
    Clause {
      consequent = fmap Value a,
      antecedent = []
    }
  ],
  extra = ()
}

infixl 5 <==

(<==) :: Atom -> Atom -> Script
head <== atom =
  Script {
    clauses = [ Clause
        { consequent = head,
          antecedent = [atom]
        }
    ],
    extra = ()
}

appendAtom :: a -> Clause' a -> Clause' a
appendAtom a Clause {..} =
  Clause
    { consequent = consequent,
      antecedent = antecedent <> [a]
    }

infixr 4 /\

(/\) :: Script-> Atom -> Script
Script clauses _ /\ atom =
  Script (fmap (appendAtom atom) clauses) ()


instance Applicative (Script' c) where
  pure = Script []
  (<*>) (Script xs f) (Script ys a) = Script (xs <> ys) (f a)

instance Monad (Script' c) where
  (>>=) (Script xs a) f =
    let Script ys b = f a
    in Script (xs <> ys) b

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


{- [markdown]
## Formating instances
-}

instance Fmt.Buildable t => Fmt.Buildable (Atom' t) where
  build Atom{..} =
    head|+ Fmt.mapF tail

instance Fmt.Buildable Term where
  build = \case
    Var v -> Fmt.build v
    Value v -> Fmt.build v

instance Fmt.Buildable Predicate where
  build Predicate{..} = Fmt.build name

instance Fmt.Buildable Label where
  build Label{..} = Fmt.build name

instance Fmt.Buildable Variable where
  build Variable{..} = Fmt.build name

instance Fmt.Buildable Value where
  build = \case
    String t -> Fmt.build t
    Int n -> Fmt.build n
    Symbol s -> Fmt.build s

instance Fmt.Buildable a => Fmt.Buildable (Clause' a) where
  build Clause{..} =
    if null antecedent
      then consequent|+"."
      else  let tail = fmap (Fmt.fmt . Fmt.build) antecedent
                        & intersperse " /\\ "
                        & concat
            in consequent|+" -| "+|tail|+"."

instance Fmt.Buildable Script where
  build Script{..} =
    fmap (\c -> "  "+|c|+"\n") clauses
    & concat
    & Fmt.build