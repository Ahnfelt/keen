module Language.Keen.Parser where

import Language.Keen.Syntax

import Text.ParserCombinators.Parsec hiding (Parser)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad.State

data ParserState = ParserState {
    parserIndentationLevel :: Maybe Int,
    parserSemicolon :: Bool
    }

type Parser a = GenParser Char ParserState a

reserved = [
    "import", "export",
    "data", "record", "type", "class", "instance",
    "operator", "operatorleft", "operatorright",
    ".", "_", "=", "->", "<-", ":", "\\",
    "//", "/*", "*/"   
    ]

math = "!@#$%&/=+?|^*-.:<>"

checkSemicolon :: Parser a -> Parser a
checkSemicolon m = do
    state <- getState
    if parserSemicolon state
        then m
        else fail "Unexpected semicolon inserted due to indentation"

parseIgnorable = do
    -- TODO: If there is an indentation level, use it to insert ';'
    many (oneOf " \r\n")
    indentationLevel <- liftM parserIndentationLevel getState
    case indentationLevel of
        Nothing -> return ()
        Just i -> do
            column <- liftM sourceColumn getPosition
            if column == i 
                then updateState (\state -> state { parserSemicolon = True })
                else if column > i
                    then return ()
                    else lookAhead eof <|> fail "Too little indentation"

-- Terminals

parseSemicolon :: Parser ()
parseSemicolon = do
    (char ';' >> return ()) <|> fake
    parseIgnorable
    updateState (\state -> state { parserSemicolon = False })
    where
        fake = do
            state <- getState
            if parserSemicolon state
                then return ()
                else fail "Semicolon expected"

parseReservedLower word = checkSemicolon $ do
    string word
    notFollowedBy alphaNum
    parseIgnorable

parseReservedMath word = checkSemicolon $ do
    string word
    notFollowedBy (oneOf math)
    parseIgnorable

parseSpecial word = do
    string word
    parseIgnorable

parseBeginEnd indentation begin end parseInside = checkSemicolon $ do
    string begin
    parseIgnorable
    column <- liftM sourceColumn getPosition
    let indentationLevel = if indentation then Just column else Nothing
    oldIndentationLevel <- liftM parserIndentationLevel getState
    updateState (\state -> state {parserIndentationLevel = indentationLevel})
    inside <- parseInside
    updateState (\state -> state {parserIndentationLevel = oldIndentationLevel})
    string end
    parseIgnorable
    return inside
    
parseBraces parseInside = parseBeginEnd True "{" "}" parseInside
parseBrackets parseInside = parseBeginEnd False "[" "]" parseInside
parseParenthesis parseInside = parseBeginEnd False "(" ")" parseInside

parseLower :: Parser String
parseLower = checkSemicolon $ do
    c <- lower
    cs <- many alphaNum
    as <- many (char '\'')
    parseIgnorable
    return ([c] ++ cs ++ as)

parseUpper :: Parser String
parseUpper = checkSemicolon $ do
    c <- upper
    cs <- many alphaNum
    as <- many (char '\'')
    parseIgnorable
    return ([c] ++ cs ++ as)

parseMath :: Parser String
parseMath = checkSemicolon $ do
    s <- many1 (oneOf math)
    parseIgnorable
    return s

parseOperator :: Parser String -> Parser String
parseOperator parseSymbol = checkSemicolon $ do
    w <- option "" (string "_")
    ss <- parseSymbol `sepBy1` (char '_')
    w' <- option "" (string "_")
    parseIgnorable
    return (w ++ intercalate "_" ss ++ w')
            
parseDelayingOperator :: Parser String -> Parser (String, [(String, Fixity)], [Bool])
parseDelayingOperator parseSymbol = checkSemicolon $ do
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


