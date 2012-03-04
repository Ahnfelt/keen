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

math = "!@#$%&/=+?|^*-.:<>\\"

noSemicolon :: Parser a -> Parser a
noSemicolon m = do
    state <- getState
    if parserSemicolon state
        then fail "Unexpected semicolon inserted due to indentation"
        else m

parseEscapeCode :: Parser Char
parseEscapeCode =
    (do string "\\\\"; return '\\') <|>
    (do string "\\\""; return '"') <|>
    (do string "\\'"; return '\'') <|>
    (do string "\\t"; return '\t') <|>
    (do string "\\r"; return '\r') <|>
    (do string "\\n"; return '\n') <?>
    "Invalid escape code"
    
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

parseReservedLower word = noSemicolon $ do
    string word
    notFollowedBy alphaNum
    parseIgnorable

parseReservedMath word = noSemicolon $ do
    string word
    notFollowedBy (oneOf math)
    parseIgnorable

parseSpecial word = do
    string word
    parseIgnorable

parseBeginEnd indentation begin end parseInside = noSemicolon $ do
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

parseIntegral :: (Num a, Read a) => Parser a
parseIntegral = do
    ds <- many1 digit
    parseIgnorable
    return (read ds)

parseFractional :: (Fractional a, Read a) => Parser a
parseFractional = do
    ds <- many1 digit
    fs <- option "" $ do
        f <- char '.'
        fs <- many1 digit 
        return (f:fs)
    es <- option "" $ do
        e <- oneOf "eE"
        ss <- option "" (do s <- oneOf "+-"; return [s])
        ds <- many1 digit 
        return (e:(ss ++ ds))
    parseIgnorable
    return (read (ds ++ fs ++ es))

parseCharacter :: Parser Char
parseCharacter = do
    char '\''
    c <- noneOf "'\\" <|> parseEscapeCode
    char '\''
    parseIgnorable
    return c

parseString :: Parser String
parseString = do
    char '"'
    cs <- many $ noneOf "\"\\" <|> parseEscapeCode
    char '"'
    parseIgnorable
    return cs

parseLower :: Parser String
parseLower = noSemicolon $ do
    c <- lower
    cs <- many alphaNum
    as <- many (char '\'')
    parseIgnorable
    return ([c] ++ cs ++ as)

parseUpper :: Parser String
parseUpper = noSemicolon $ do
    c <- upper
    cs <- many alphaNum
    as <- many (char '\'')
    parseIgnorable
    return ([c] ++ cs ++ as)

parseMath :: Parser String
parseMath = noSemicolon $ do
    s <- many1 (oneOf math)
    parseIgnorable
    return s

parseOperator :: Parser String -> Parser String
parseOperator parseSymbol = noSemicolon $ do
    w <- option "" (string "_")
    ss <- parseSymbol `sepBy1` (char '_')
    w' <- option "" (string "_")
    parseIgnorable
    return (w ++ intercalate "_" ss ++ w')
            
parseDelayingOperator :: Parser String -> Parser (String, [(String, Fixity)], [Bool])
parseDelayingOperator parseSymbol = noSemicolon $ do
    w <- optionMaybe parseWildcard
    sws <- many1 (try (do s <- parseSymbol; w <- parseWildcard; return (s, w)))
    s <- option "" parseSymbol
    let name = (if w == Nothing then "" else "_") ++ concatMap (\(s, w) -> s ++ if w then "__" else "_") sws ++ s
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


