module Main where

import Text.Parsec
import Data.Maybe (isJust, fromJust)
import Debug.Trace

main = putStrLn "Hello World!"

-- should CM keep or drop the text between tags
data Keep = Keep | Drop
    deriving (Show)

data CMTag = EOF | Close String | Tag String CMTag Keep
    deriving (Show)

-- critic markup tags
cmAdd = Tag "{++" (Close "++}") Keep
cmDel = Tag "{--" (Close "--}") Drop
cmSub = Tag "{~~" (Tag "~>" (Close "~~}") Keep) Drop
cmHil = Tag "{==" (Close "==}") Keep
cmCom = Tag "{>>" (Close "<<}") Drop

cmTags :: [CMTag]
cmTags = [cmAdd,cmDel,cmSub,cmHil,cmCom]

-- | builds the stop characters
stopChars :: String -> CMTag -> String
stopChars stops EOF = stops
stopChars stops (Close str) = addHead stops str
stopChars stops (Tag str _ _) = addHead stops str

-- | prepends the head of a possibly empty string to another string
addHead :: String -> String -> String
addHead str "" = str
addHead str (x:xs) = x:str

-- | keep or drop results of the parser
-- n.b. have to make sure to run the parser even when dropping its results
keepFilter :: Keep -> Parsec String () String -> Parsec String () String
keepFilter Keep x = x
keepFilter Drop x = x >> return ""

matchTag :: CMTag -> Parsec String () String
matchTag EOF = eof >> return ""
matchTag (Close x) = try $ string x >> return ""
matchTag (Tag start stop keep) = do
    try $ string start
    pre <- keepFilter keep (many $ noneOf stopChars')
    post <- matchTag stop <|>                   -- match the end tag
        choice (map innerTag cmTags) <|>        -- match a new tag, and continue parsing
        choice (map loneStopChar stopChars')    -- match a lone stop character, and continue parsing
    return (pre ++ post)
    where
        tagStarts = "{~"
        stopChars' = stopChars tagStarts stop
        restTag = matchTag (Tag "" stop keep)
        loneStopChar x = do
            char x
            rest <- restTag
            keepFilter keep $ return (x:rest)
        innerTag tag = do
            inner <- matchTag tag
            rest <- restTag
            keepFilter keep $ return (inner ++ rest)

cmParse :: Parsec String () String
cmParse = matchTag (Tag "" EOF Keep)

{-cmStart' :: CMTag -> Parsec String () String-}
{-cmStart' (Tag )-}

{- what's the structure of a critic markup document?
 - n.b. there is not actually a spec wrt:
 -      1. escaping tags in text: e.g. how to insert a literal "{++"?
 -      2. overlapping tags: "{++ lorem {>> ipsum ++} dolor <<}" parses to what?
 - let's ban overlaps, and no escaping for now
 - structure:
 -      text -> tag + text -> recurse
 -}
cmStart :: Maybe String -> Parsec String () String
cmStart Nothing = do
    pre <- many (noneOf "{")
    -- we've hit the typical break
    (inner, post) <- cmRest Nothing
    return (concat [pre,inner,post])
cmStart (Just end@(x:xs)) = do
    pre <- many (noneOf $ x:"{")
    -- we stopped because: we ended the tag, we matched the end of tag char, or the usual break
    (try $ string end >> return pre) <|>
        (try $ char x >> cmStart (Just end) >>= \post -> return (concat [pre,[x],post])) <|>
        (cmRest (Just end) >>= \(inner,post) -> return (concat [pre,inner,post]))

-- the usual break
-- we've hit eof, or an open brace
-- check for valid eof
-- check for open tags
-- match open brace
cmRest :: Maybe String -> Parsec String () (String,String)
cmRest end = eofCheck <|> do
    inner <- cmAdd' <|> (string "{")
    post <- cmStart end
    return (inner,post)
    where
        eofCheck = eof >> case end of
            Nothing -> return ("","")
            Just x -> unexpected ("end (no CM closing tag: " ++ x ++ ")")

cmAdd' :: Parsec String () String
cmAdd' = do
    try $ string "{++"
    cmStart (Just "++}")


{-cmadd :: Parsec String () String-}
{-cmadd = do-}
    {-string "++"-}
    {-pre <- -}

runTests = sequence_ $ map (outstr) tests
    where
        outstr x = putStr ("\"" ++ x ++ "\" :=>: ") >> (parseTest cmParse x)

tests = [
    "test",
    "test {++foo++} test",
    "test {--foo--} test",
    "test {--{++foo++}bar--} test",
    "test {++{--foo--}bar++} test",
    "test {~~foo~>bar~~} test",
    "test {~~foo{++baz++}~>bar~~} test",
    "test {~~foo{--baz--}~>bar~~} test",
    "test {++foo",
    "test"
    ]

{-tests = [-}
    {-("{++test++}","test"),-}
    {-("{++test{++tester++}++}","test{++tester++}")]-}



