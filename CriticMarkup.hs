{-# LANGUAGE QuasiQuotes #-}

module Main where

import Text.Parsec
import Control.Monad (when)
import Data.Maybe (fromJust, listToMaybe, catMaybes)
import System.Environment (getArgs)
import System.Console.Docopt
import System.IO (stderr, hPutStrLn)
import System.IO.Unsafe (unsafePerformIO)
{-import Data.Maybe (isJust, fromJust)-}
{-import Debug.Trace-}

patterns :: Docopt
patterns = [docoptFile|USAGE|]

main'' = putStrLn "Hello World!"
main' = do
    (infile:outfile:_) <- getArgs
    contents <- readFile infile
    let res = parse cmParse "" contents
    case res of
        Left err -> putStrLn $ show err
        Right res' -> writeFile outfile res'
main = do
    args <- parseArgsOrExit patterns =<< getArgs

    when (isPresent args (longOption "help")) $ do
        exitWithUsage patterns

    when ((isPresent args (argument "INFILE")) || (isPresent args (argument "OUTFILE"))) $ do
        hPutStrLn stderr deprecationWarning

    contents <- input args
    case (parse cmParse "" contents) of
        Left err -> putStrLn $ show err
        Right out -> output args out
    where
        deprecationWarning = "`criticmarkuphs INFILE OUTFILE` is deprecated\nSee `criticmarkuphs -h`"
        maybeFile args lo ar = listToMaybe $ catMaybes [getArg args (longOption lo), getArg args (argument ar)]
        input args = case (maybeFile args "infile" "INFILE") of
            Just file -> readFile file
            Nothing -> getContents
        output args out = case (maybeFile args "outfile" "OUTFILE") of
            Just file -> writeFile file out
            Nothing -> putStr out
    

-- should CM keep or drop the text between tags
data Keep = Keep | Drop
    deriving (Show)

-- 3 types of tages
-- EOF tag
-- "terminator" tag, containing string indicating end of tag
-- regular tag, containing:
--  . open tag string
--  . transition tag ending current tag
--  . keep/drop text in tag
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
        loneStopChar x = innerParse (char x >> return [x])
        innerTag tag = innerParse (matchTag tag)
        innerParse x = do
            inner <- keepFilter keep $ x
            rest <- restTag
            return (inner ++ rest)

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
{-cmStart :: Maybe String -> Parsec String () String-}
{-cmStart Nothing = do-}
    {-pre <- many (noneOf "{")-}
    {--- we've hit the typical break-}
    {-(inner, post) <- cmRest Nothing-}
    {-return (concat [pre,inner,post])-}
{-cmStart (Just end@(x:xs)) = do-}
    {-pre <- many (noneOf $ x:"{")-}
    {--- we stopped because: we ended the tag, we matched the end of tag char, or the usual break-}
    {-(try $ string end >> return pre) <|>-}
        {-(try $ char x >> cmStart (Just end) >>= \post -> return (concat [pre,[x],post])) <|>-}
        {-(cmRest (Just end) >>= \(inner,post) -> return (concat [pre,inner,post]))-}

runTests = sequence_ $ map (outstr) tests
    where
        outstr' x = putStr ("\"" ++ x ++ "\" :=>: ") >> (parseTest cmParse x)
        outstr (x,y) = do
            let res = parse cmParse "" x
            case res of
                Left err -> putStrLn ("Error :: \"" ++ x ++ "\" :=>: " ++ (show err))
                Right res' -> if res' == y then putStr "" else putStrLn ("Misparse :: \"" ++ x ++ "\" :=>: \"" ++ res' ++ "\"")

tests = [
    ("test","test"),
    ("test {++foo++} test","test foo test"),
    ("test {--foo--} test","test  test"),
    ("test {--{++foo++}bar--} test","test  test"),
    ("test {++{--foo--}bar++} test","test bar test"),
    ("test {~~foo~>bar~~} test","test bar test"),
    ("test {~~foo{++baz++}~>bar~~} test","test bar test"),
    ("test {~~foo{--baz--}~>bar~~} test","test bar test"),
    ("test {++foo",""),
    ("test","test")
    ]

{-tests = [-}
    {-("{++test++}","test"),-}
    {-("{++test{++tester++}++}","test{++tester++}")]-}



