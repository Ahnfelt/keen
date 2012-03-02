module Language.Keen.Parser where

import Language.Keen.Syntax

import Text.Parsec

data Symbol = Symbol {
    name :: String,
    members :: [String],
    implicit :: Bool
    }

data Port = Port {
    alias :: Maybe String,
    package :: String,
    symbols :: [Symbol]
    }

data Definition
    = ValueDefinition String Expression
    | TypeDefinition String [String] Type
    | DataDefinition String [String] [(String, [Type])]
    | RecordDefinition String [String] [(String, Type)]

data Module = Module {
    imports :: [Port],
    exports :: [Port],
    definitions :: [Definition]
    }


