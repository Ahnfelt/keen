module Language.Keen.Parser where

import Language.Keen.Syntax

import Text.ParserCombinators.Parsec hiding (Parser)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

type Parser a = GenParser Char () a

parseModule :: Parser Module
parseModule = return undefined

parseLower :: Parser String
parseLower = do
    c <- lower
    cs <- many alphaNum
    as <- many (char '\'')
    return ([c] ++ cs ++ as)

parseUpper :: Parser String
parseUpper = do
    c <- upper
    cs <- many alphaNum
    as <- many (char '\'')
    return ([c] ++ cs ++ as)

parseMath :: Parser String
parseMath = many1 (oneOf "!@#$%&/=+?|^*-.:<>")

parseOperator :: Parser String -> Parser String
parseOperator parseSymbol = do
    w <- option "" (string "_")
    ss <- parseSymbol `sepBy1` (char '_')
    w' <- option "" (string "_")
    return (w ++ intercalate "_" ss ++ w')
            
parseDelayingOperator :: Parser String -> Parser (String, [Bool])
parseDelayingOperator parseSymbol = do
    w <- optionMaybe parseWildcard
    ss <- many1 (try (do s <- parseSymbol; w <- parseWildcard; return (s, w)))
    s <- option "" parseSymbol
    let name = (if w == Nothing then "" else "_") ++ concatMap ((++ "_") . fst) ss ++ s
    let delays = fromMaybe [] (fmap (:[]) w) ++ map snd ss
    return (name, delays)
    where
        parseWildcard = (string "_" >> return False) <|> (string "\\_" >> return True)

