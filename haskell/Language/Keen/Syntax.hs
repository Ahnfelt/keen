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
    | Character Char
    | Unit
    deriving (Eq, Show, Read)

data Expression
    = Unparsed [Expression]
    | Value Value
    | Variable Name
    | Annotate Expression Type
    | Apply Expression Expression
    | Let [Binding] Expression
    deriving (Eq, Show, Read)

data Type
    = UnparsedType [Type]
    | FlexibleType Name
    | RigidType Name
    | FunctionType Type Type
    | NumberType
    | StringType
    | CharacterType
    | UnitType
    deriving (Eq, Show, Read)

data Forall = Forall [Name] Type
    deriving (Eq, Show, Read)


data Symbol = Symbol {
    name :: String,
    members :: [String],
    implicit :: Bool
    }
    deriving Show

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
    deriving Show

data Definition
    = ValueDefinition Binding
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
    | InstanceDefinition {
        instanceType :: Forall,
        instanceExpression :: Expression
    }
    | OperatorDefinition {
        operatorName :: String ,
        operatorFixity :: [(String, Fixity)],
        operatorDelay :: [Bool] 
    }
    deriving Show

data Module = Module {
    imports :: [Port],
    exports :: [Port],
    definitions :: [Definition]
    } 
    deriving Show

lambda x e = Value (Closure Map.empty x e)

