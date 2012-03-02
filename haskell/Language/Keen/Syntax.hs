module Language.Keen.Syntax where

import qualified Data.Map as Map
import Data.Map (Map)

type Name = String
type Environment = Map Name Value

data Binding = Binding {
    bindingType :: Maybe Forall,
    bindingName :: String,
    bindingExpression :: Expression
    } deriving (Eq, Show, Read)

data Value
    = Closure Environment Name Expression
    | Number Double
    | String String
    | Unit
    deriving (Eq, Show, Read)

data Expression
    = Unparsed Expression Expression
    | Value Value
    | Variable Name
    | Annotate Expression Type
    | Apply Expression Expression
    | Let [Binding] Expression
    deriving (Eq, Show, Read)

data Type
    = UnparsedType Type Type
    | FlexibleType Name
    | RigidType Name
    | FunctionType Type Type
    | NumberType
    | StringType
    | UnitType
    deriving (Eq, Show, Read)

data Forall = Forall [Name] Type
    deriving (Eq, Show, Read)


data Symbol = Symbol {
    name :: String,
    members :: [String],
    implicit :: Bool
    }

data Fixity 
    = Prefix 
    | Suffix 
    | Infix 
    deriving (Show, Eq)

data Port = Port {
    alias :: Maybe String,
    package :: String,
    symbols :: [Symbol]
    }

data Definition
    = ValueDefinition {
        valueType :: Maybe Forall,
        valueName :: String,
        valueExpression :: Expression
    }
    | TypeDefinition {
        typeName :: String, 
        typeVariables :: [String], 
        typeType :: Type
    }
    | DataDefinition {
        dataName :: String, 
        dataTypeVariables :: [String], 
        dataConstructors :: [(String, [Type])]
    }
    | RecordDefinition {
        recordName :: String,
        recordTypeVariables :: [String],
        recordFields :: [(String, Forall)]
    }
    | OperatorDefinition {
        operatorName :: String ,
        operatorFixity :: [(String, Fixity)],
        operatorDelay :: [Bool] 
    }

data Module = Module {
    imports :: [Port],
    exports :: [Port],
    definitions :: [Definition]
    }

