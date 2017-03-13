import System.IO
import Data.Char
import Data.Map as M
import Data.Set as S
import Data.List as L
import Debug.Trace (trace)


main = do 
    let handle = hGetContents stdin
    string <- handle
    let output = process string
    putStrLn output


process :: String -> String
process xs = let listOfTokens = organize (tokenize xs)
                 linesInFile = lines xs
                 includesLines = takeWhile lineStartsWithHashSign linesInFile
                 contentLines = dropWhile lineStartsWithHashSign linesInFile
                 content =  processLine listOfTokens $ unlines contentLines
                 defines = defineTokens listOfTokens
             in unlines includesLines ++ unlines defines ++ "\n\n\n"  ++ content -- add three empty lines
             
            -- unlines $ processLines listOfTokens $ lines $  dropWhile lineStartsWithHashSign $lines xs

----inserts defines into lines, assuming liness already underscorlized
--processLines :: [Token] -- list of known tokens
--                -> [String] -- list of lines
--                -> [String] --resulting lines
--processLines tokens lines = let underscoreLized = lines -- L.map (processLine tokens) lines :: [String]
--                                -- insert define
--                            in takeWhile lineStartsWithHashSign underscoreLized 
--                            ++ defineTokens tokens
--                            ++ dropWhile lineStartsWithHashSign underscoreLized

-- get #defines for tokens
defineTokens :: [Token] -> [String]
defineTokens xs = L.filter (/=[]) $ L.map getDefineForTag xs
                  where getDefineForTag :: Token -> String
                        -- only define when token is valid id
                        getDefineForTag tk 
                            | isTokenValidID tk = "#define " 
                                ++ replicate (index tk+1) '_' 
                                ++ " " 
                                ++ getTokenContent tk
                            | otherwise = []
                        index tk = case tk `elemIndex` xs of 
                            Just idx -> idx
                            Nothing -> error "Global Define :: Token Not Found"

-- if line starts with hashSign, empty lines and #include, #define , comments are considered true
lineStartsWithHashSign :: String -> Bool
lineStartsWithHashSign [] = True
lineStartsWithHashSign ('#':_) = True
lineStartsWithHashSign ('/':'/':_) = True
lineStartsWithHashSign _ = False


processLine :: [Token]  -- global token table
                -> String -- current processing line
                -> String -- result of processing line
processLine _ [] = []
processLine _ ('#':xs) = '#':xs
processLine tokens xs = let curLineTokens = tokenize xs
                        in concat $ L.map (mapToken tokens) curLineTokens

-- convert a particular token to underscore
mapToken :: [Token] -- global token table
           -> Token -- a particular token to match
           -> String -- the string representation of the token, underscores
mapToken tokens tk = case tk `elemIndex` tokens of
                        Just idx -> if isTokenValidID tk  -- only substitute when token is underscore
                                    then ' ' : replicate (idx+1) '_' ++ " " -- is id, put space and underscore
                                    else getTokenContent tk -- not token, just put the original string
                        Nothing -> error $ "Token Not Found, Check implementation logic" ++ show tk


data Token = Token 
                    String -- string content
                    Bool  -- True if the token is valid ID([a-zA-Z0-9], false otherwise
                    deriving (Show, Ord, Eq)

-- parses and gets the token content that is compatible with C Macro
getTokenContent :: Token -> String
getTokenContent (Token s _) = s
isTokenValidID :: Token -> Bool
isTokenValidID (Token _ b) = b
--getTokenContent (Token s) = if length s /= 1 
--                            then s 
--                            else
--                                case head s of
--                                    '{' -> "<%"
--                                    '}' -> "%>"
--                                    '[' -> "<:"
--                                    ']' -> ":>"
--                                    '#' -> "%:"
--                                    _ -> s


tokenize :: String -> [Token]
tokenize [] = []
tokenize (x:xs)
    | isValidID x = Token (x:(takeWhile isValidID xs)) True: tokenize (dropWhile isValidID xs)
    | x == '\n' = Token "\n" False : tokenize xs
    | isSpace x = tokenize xs
    | isQuote x = Token (x:(fst $ quoteEscape xs x)) False : tokenize ( snd $ quoteEscape xs x)
    -- ignore # until end of sentence, using quote escape
    | x == '#' = Token (x:(fst $ quoteEscape xs '\n')) False : tokenize ( snd $ quoteEscape xs '\n')
    | x == '/' && xs == [] = []
    | x == '/' && head xs == '/' = Token (x:(fst $ quoteEscape xs '\n')) False : tokenize ( snd $ quoteEscape xs '\n')
    | x == '/' && head xs == '*' = Token ('/':'*':(fst $ multiLineCommentEscape xs)) False 
                                                    : tokenize (snd $ multiLineCommentEscape xs)
    | otherwise = Token [x] False: tokenize xs

isValidID :: Char -> Bool
isValidID x = (isAlphaNum x ) || ( x == '_')

isQuote :: Char -> Bool
isQuote x = x == '\"' || x == '\''

multiLineCommentEscape :: String -> (String, String)
multiLineCommentEscape str = multiLineCommentEscapeRec ("", str)
        where multiLineCommentEscapeRec :: (String, String) -> (String, String)
              multiLineCommentEscapeRec (a, []) = (a, []) -- should not happen
              multiLineCommentEscapeRec (a, ('*':'/':xs)) = (a ++ "*/", xs)
              multiLineCommentEscapeRec (a, (x:xs)) = multiLineCommentEscapeRec (a ++ [x], xs)


--quote escape :: hel, fjis\"jisjv"sub -> " -> (hel, fjis\"jisjv", sub)
quoteEscape :: String -> Char -> (String, String) -- string -> " or ' -> inquote, afterquote
quoteEscape str q = quoteEscapeRec ("", str) 
                  where quoteEscapeRec :: (String, String) -> (String, String)
                        --single escape
                        quoteEscapeRec (a,('\\':x:xs)) = quoteEscapeRec (a ++ ['\\'] ++ [x], xs)
                        quoteEscapeRec (a,(x:xs)) = if x == q then (a++[q], xs) else quoteEscapeRec (a++[x], xs)
                        quoteEscapeRec (a, [] ) = (a, []) -- should not happen


organize :: [Token] -> [Token]
organize xs = let sorted = L.sortBy (\x y -> tokenOrdering x y xs) xs -- sort list DESCENDING
                  setSorted = removeRepeat sorted
              in L.filter isTokenValidID setSorted ++ L.filter (not . isTokenValidID ) setSorted

-- return the token orderings
tokenOrdering :: Token -> Token -> [Token] -> Ordering
tokenOrdering x y xs = let countOrder = compare (length $ elemIndices y xs) 
                                        (length $ elemIndices x xs)
                       in case countOrder of 
                            EQ -> compare x y
                            x -> x

removeRepeat :: (Eq a, Show a, Ord a) => [a] -> [a]
removeRepeat xs = L.map head . group $ xs
