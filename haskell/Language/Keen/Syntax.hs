module Language.Keen.Syntax where

import qualified Data.Map as Map
import Data.Map (Map)

type Name = String
type Environment = Map Name Value

data Value
    = Closure Environment Name Expression
    | Number Double
    | String String
    | Unit
    deriving (Eq, Show, Read)

data Expression
    = Value Value
    | Variable Name
    | Annotate Expression Type
    | Apply Expression Expression
    | Let [(Name, Expression)] Expression
    deriving (Eq, Show, Read)

data Type
    = FlexibleType Name
    | RigidType Name
    | FunctionType Type Type
    | NumberType
    | StringType
    | UnitType
    deriving (Eq, Show, Read)

