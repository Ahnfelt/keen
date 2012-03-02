module Language.Keen.Parser where

import Language.Keen.Syntax

import Text.ParserCombinators.Parsec hiding (Parser)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Parser a = GenParser Char () a

reserved = [
    "import", "export",
    "data", "record", "type", "class", "instance",
    "operator", "operatorleft", "operatorright",
    ".", "_", "=", "->", "<-", ":", "\\",
    "//", "/*", "*/"   
    ]

parseIgnorable = do
    many (oneOf " \r\n")
    return ()

-- Terminals

parseWord word = do
    string word
    parseIgnorable

parseLower :: Parser String
parseLower = do
    c <- lower
    cs <- many alphaNum
    as <- many (char '\'')
    parseIgnorable
    return ([c] ++ cs ++ as)

parseUpper :: Parser String
parseUpper = do
    c <- upper
    cs <- many alphaNum
    as <- many (char '\'')
    parseIgnorable
    return ([c] ++ cs ++ as)

parseMath :: Parser String
parseMath = do
    s <- many1 (oneOf "!@#$%&/=+?|^*-.:<>")
    parseIgnorable
    return s

parseOperator :: Parser String -> Parser String
parseOperator parseSymbol = do
    w <- option "" (string "_")
    ss <- parseSymbol `sepBy1` (char '_')
    w' <- option "" (string "_")
    parseIgnorable
    return (w ++ intercalate "_" ss ++ w')
            
parseDelayingOperator :: Parser String -> Parser (String, [(String, Fixity)], [Bool])
parseDelayingOperator parseSymbol = do
    w <- optionMaybe parseWildcard
    sws <- many1 (try (do s <- parseSymbol; w <- parseWildcard; return (s, w)))
    s <- option "" parseSymbol
    let name = (if w == Nothing then "" else "_") ++ concatMap ((++ "_") . fst) sws ++ s
    let delays = fromMaybe [] (fmap (:[]) w) ++ map snd sws
    let symbols = map fst sws ++ if s /= "" then [s] else []
    let suffix = ((w /= Nothing) : repeat True)
    let prefix = (replicate (length sws) True ++ if s /= "" then [False] else [])
    let fix p s = if p && s then Infix else if p then Prefix else if s then Suffix else error "No fixity"
    parseIgnorable
    return (name, zip symbols (zipWith fix prefix suffix), delays)
    where
        parseWildcard = char '_' >> option False (char '_' >> return True)

-- Non-terminals

parseModule :: Parser Module
parseModule = do
    parseIgnorable
    return undefined


