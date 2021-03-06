module Language.Keen.Parser where

import Language.Keen.Syntax
import Debug.Trace

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
    "type", "data", "record", "instance",
    "operator", "operatorleft", "operatorright",
    ".", "=", "->", "<-", ":", "\\",
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

parseIgnorableAlways = many (oneOf " \r\n")

parseIgnorable = do
    parseIgnorableAlways
    indentationLevel <- liftM parserIndentationLevel getState
    case indentationLevel of
        Nothing -> return ()
        Just i -> do
            column <- liftM sourceColumn getPosition
            if column == i 
                then updateState (\state -> state { parserSemicolon = True })
                else if column > i
                    then return ()
                    else lookAhead ending <?> "more indentation"
    where
        ending = ((char '}' <|> char ']' <|> char ')') >> return ()) <|> eof

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
                else fail "expecting ';'"

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
    parseIgnorableAlways
    string end
    parseIgnorable
    return inside

parseBraces parseInside = parseBeginEnd True "{" "}" parseInside
parseBrackets parseInside = parseBeginEnd False "[" "]" parseInside
parseParenthesis parseInside = parseBeginEnd False "(" ")" parseInside

parseIntegral :: (Num a, Read a) => Parser a
parseIntegral = noSemicolon $ do
    ds <- many1 digit
    parseIgnorable
    return (read ds)

parseFractional :: (Fractional a, Read a) => Parser a
parseFractional = noSemicolon $ try $ do
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
parseCharacter = noSemicolon $ do
    char '\''
    c <- noneOf "'\\" <|> parseEscapeCode
    char '\''
    parseIgnorable
    return c

parseString :: Parser String
parseString = noSemicolon $ do
    char '"'
    cs <- many $ noneOf "\"\\" <|> parseEscapeCode
    char '"'
    parseIgnorable
    return cs

parseLower :: Parser String
parseLower = noSemicolon $ do
    x <- try $ do
        c <- lower
        cs <- many alphaNum
        as <- many (char '\'')
        let x = [c] ++ cs ++ as
        when (x `elem` reserved) $ fail $ "Unexpeced reserved symbol '" ++ x ++ "'"
        return x
    parseIgnorable
    return x

parseUpper :: Parser String
parseUpper = noSemicolon $ do
    c <- upper
    cs <- many alphaNum
    as <- many (char '\'')
    let x = [c] ++ cs ++ as
    parseIgnorable
    return x

parseMath :: Parser String
parseMath = noSemicolon $ do
    x <- try $ do
        x <- many1 (oneOf math)
        when (x `elem` reserved) $ fail $ "Unexpeced reserved symbol '" ++ x ++ "'"
        return x
    parseIgnorable
    return x

parseOperator :: Parser String -> Parser String
parseOperator parseSymbol = noSemicolon $ do
    parts <- many1 (parseSymbol <|> string "_")
    parseIgnorable
    return (concat parts)

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
    (is, es) <- parseImportsExports
    ds <- many parseDefinition
    parseIgnorable
    eof
    return Module { imports = is, exports = es, definitions = ds }

parseImportsExports :: Parser ([Port], [Port])
parseImportsExports = return ([], [])

parseDefinition :: Parser Definition
parseDefinition = 
    parseOperatorDefinition <|>
    liftM ValueDefinition parseBinding

parseOperatorDefinition :: Parser Definition
parseOperatorDefinition = do
    associativity <- parseAssociativity
    (symbol, parts, delays) <- parseDelayingOperator parseSymbol
    (less, same, greater) <- option ([], [], []) (parseBraces (parsePrecedence [] [] []))
    return $ OperatorDefinition symbol parts delays associativity less same greater
    where
        parseAssociativity = try $
            (parseReservedLower "operator" >> return NonAssociative) <|>
            (parseReservedLower "operatorleft" >> return LeftAssociative) <|>
            (parseReservedLower "operatorright" >> return RightAssociative)
        parsePrecedence less same greater = do
            f <- optionMaybe $
                (parseReservedMath "<" >> 
                    return (\operators -> parsePrecedence (less ++ operators) same greater)) <|>
                (parseReservedMath "=" >> 
                    return (\operators -> parsePrecedence less (same ++ operators) greater)) <|>
                (parseReservedMath ">" >> 
                    return (\operators -> parsePrecedence less same (greater ++ operators)))
            case f of
                Just f -> do
                    operators <- parseOperator parseSymbol `sepBy1` parseReservedMath ","
                    optional parseSemicolon
                    f operators
                Nothing -> return (less, same, greater)

parseSymbol = parseLower <|> parseMath <|> parseUpper

parseBinding :: Parser Binding
parseBinding = do
    a <- optionMaybe parseBindingType
    p <- parsePattern
    parseReservedMath "="
    e <- parseExpression
    return (Binding a p e)
    
parseBindingType :: Parser Forall
parseBindingType = do
    parseReservedMath ":"
    fail "Binding type parser not implemented"

parsePattern :: Parser String
parsePattern = parseLower

parseExpression :: Parser Expression
parseExpression = do
    unparsed <- many1 $
        liftM Variable (parseOperator parseSymbol) <|> 
        parseLiteral <|> parseParenthesis parseExpression <|> parseBlock
    case unparsed of
        [e] -> return e
        _ -> return (Unparsed unparsed)

parseLiteral =
    parseLambda <|>
    liftM (Value . Character) parseCharacter <|>
    liftM (Value . String) parseString <|>
    liftM (Value . Number) parseFractional <|>
    liftM (Value . Number) parseIntegral

parseLambda = do
    parseReservedMath "\\"
    p <- parsePattern
    parseReservedMath "->"
    e <- parseExpression
    return (lambda p e)

parseBlock = parseBraces $ do
    e <- parseInsideBlock
    optional parseSemicolon
    return e

parseInsideBlock = parseLet <|> parseMonadSugar

parseLet = do
    bindings <- many1 parseBind
    e <- parseInsideBlock
    return (Let bindings e)

parseBind = do
    t <- optionMaybe parseBindingType
    p <- try $ do
        p <- parsePattern
        parseReservedMath "="
        return p
    e <- parseExpression
    parseSemicolon
    return (Binding t p e)

parseMonadSugar = do
    p <- optionMaybe $ try $ do
        p <- parsePattern
        parseReservedMath "<-"
        return p
    e <- parseExpression
    e' <- optionMaybe $ do
        try (parseSemicolon >> notFollowedBy (char '}'))
        parseInsideBlock
    case (p, e') of
        (Nothing, Nothing) -> return e
        (Nothing, Just e') -> return (Apply (Apply (Variable "_>>_") e) e')
        (Just p, Just e') -> return (Apply (Apply (Variable "_>>=_") e) (lambda p e'))
        (Just _, Nothing) -> fail "something after the last bind (<-)"


