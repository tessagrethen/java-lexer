{- Name: Tessa Pham
   File: Lexer.hs
   Description: Implements a Java lexer.
-}

module Main where
import Data.Char
import System.Environment
import System.Exit
import System.FilePath
main :: IO ()
main = do
  args <- getArgs
  filename <- checkArgs args
  input <- readFile filename
  let result = lexJava input
  writeFile (takeBaseName filename <.> "lex") (unlines result)

checkArgs :: [String] -> IO FilePath
checkArgs [path] = pure path
checkArgs _other = do
  putStrLn "Usage: ./Lexer <filename>.java"
  putStrLn "Writes to <filename>.lex"
  exitFailure

isUnderscore :: Char -> Bool
isUnderscore '_' = True
isUnderscore _   = False

isDollarSign :: Char -> Bool
isDollarSign '$' = True
isDollarSign _   = False

isApostrophe :: Char -> Bool
isApostrophe '\'' = True
isApostrophe _    = False

isBackslash :: Char -> Bool
isBackslash '\\' = True
isBackslash _    = False

isSlash :: Char -> Bool
isSlash '/' = True
isSlash _   = False

isStar :: Char -> Bool
isStar '*' = True
isStar _   = False

isDoubleQuote :: Char -> Bool
isDoubleQuote '"' = True
isDoubleQuote _   = False

isZero :: Char -> Bool
isZero '0' = True
isZero _   = False

isIntegerTypeSuffix :: Char -> Bool
isIntegerTypeSuffix 'l' = True
isIntegerTypeSuffix 'L' = True
isIntegerTypeSuffix _   = False

isBinDigit :: Char -> Bool
isBinDigit '0' = True
isBinDigit '1' = True
isBinDigit _   = False

isJavaLetter :: Char -> Bool
isJavaLetter c
  | isAlpha c || isUnderscore c || isDollarSign c = True
  | otherwise                                     = False

isJavaLetterOrDigit :: Char -> Bool
isJavaLetterOrDigit c
  | isJavaLetter c || isDigit c = True
  | otherwise                   = False

isNonZeroDigit :: Char -> Bool
isNonZeroDigit c
  | isDigit c && isZero c == False = True
  | otherwise                      = False

isUnicodeMarker :: Char -> Bool
isUnicodeMarker 'u' = True
isUnicodeMarker _   = False

isZeroToThree :: Char -> Bool
isZeroToThree '0' = True
isZeroToThree '1' = True
isZeroToThree '2' = True
isZeroToThree '3' = True
isZeroToThree _   = False

giveIdentifier :: [Char] -> [Char]
giveIdentifier [] = []
giveIdentifier (c:str)
  | isJavaLetterOrDigit c       = c : giveIdentifier str
  | otherwise                   = []

giveUnderscoreSequence :: [Char] -> [Char]
giveUnderscoreSequence [c]
  | isUnderscore c = [c]
  | otherwise = []
giveUnderscoreSequence (c:str)
  | isUnderscore c = c : giveUnderscoreSequence str
  | otherwise    = []

whatFollows :: String -> String -> Char
whatFollows substr str
  | (cutString substr str) /= [] = head (cutString substr str)
  | otherwise = ' '

giveDecimalNumeral :: [Char] -> [Char]
giveDecimalNumeral [] = []
giveDecimalNumeral [c]
  | isDigit c             = [c]
  | isIntegerTypeSuffix c = [c]
  | otherwise = []
giveDecimalNumeral (c:str)
  | isDigit c = c : giveDecimalNumeral str
  | isUnderscore c =
    case x of
      True -> giveUnderscoreSequence (c:str) ++ giveDecimalNumeral (cutString (giveUnderscoreSequence (c:str)) (c:str))
      False -> []
  | isIntegerTypeSuffix c       = [c]
  | otherwise                   = []
  where
      x = isDigit (whatFollows (giveUnderscoreSequence (c:str)) (c:str))

-- isHexNumeral checks if the first two characters after 0 imply the token could be a HexNumeral.
isHexNumeral :: [Char] -> Bool
isHexNumeral []  = False
isHexNumeral [c] = False
isHexNumeral (c1:c2:str)
  | (c1 == 'x' || c1 == 'X') && isHexDigit c2 = True
  | otherwise                                 = False

-- After passing the isHexNumeral test, the input for giveHexNumeral will always have a hex digit as its first character.
giveHexNumeral :: [Char] -> [Char]
giveHexNumeral [] = []
giveHexNumeral [c]
  | isHexDigit c          = [c]
  | isIntegerTypeSuffix c = [c]
  | otherwise             = []
giveHexNumeral (c:str)
  | isHexDigit c = c : giveHexNumeral str
  | isUnderscore c =
    case x of
      True -> giveUnderscoreSequence (c:str) ++ giveHexNumeral (cutString (giveUnderscoreSequence (c:str)) (c:str))
      False -> []
  | isIntegerTypeSuffix c          = [c] 
  | otherwise                      = []
  where
    x = isHexDigit (whatFollows (giveUnderscoreSequence (c:str)) (c:str))

isOctalNumeral :: [Char] -> Bool
isOctalNumeral [] = False
isOctalNumeral (c:str)
  | isOctDigit c || isUnderscore c = True
  | otherwise                      = False

giveOctalNumeral :: [Char] -> [Char]
giveOctalNumeral [] = []
giveOctalNumeral [c]
  | isOctDigit c          = [c]
  | isIntegerTypeSuffix c = [c]
  | otherwise             = []
giveOctalNumeral (c:str)
  | isOctDigit c = c : giveOctalNumeral str
  | isUnderscore c =
    case x of
      True -> giveUnderscoreSequence (c:str) ++ giveOctalNumeral (cutString (giveUnderscoreSequence (c:str)) (c:str))
      False -> []
  | isIntegerTypeSuffix c          = [c]
  | otherwise                      = []
  where
    x = isOctDigit (whatFollows (giveUnderscoreSequence (c:str)) (c:str))
    
-- isBinaryNumeral checks if the first two characters after 0 imply the token could be a BinaryNumeral.
isBinaryNumeral :: [Char] -> Bool
isBinaryNumeral []  = False
isBinaryNumeral [c] = False
isBinaryNumeral (c1:c2:str)
  | (c1 == 'b' || c1 == 'B') && isBinDigit c2 = True
  | otherwise                                 = False

-- After passing the isBinaryNumeral test, the input for giveBinaryNumeral will always have a binary digit as its first character.
giveBinaryNumeral :: [Char] -> [Char]
giveBinaryNumeral [] = []
giveBinaryNumeral [c]
  | isBinDigit c          = [c]
  | isIntegerTypeSuffix c = [c]
  | otherwise             = []
giveBinaryNumeral (c1:c2:str)
  | isBinDigit c1                    = c1 : giveBinaryNumeral (c2:str)
  | isUnderscore c1 && isBinDigit c2 = [c1] ++ [c2] ++ giveBinaryNumeral str
  | otherwise                        = []

isSingleCharacter :: [Char] -> Bool
isSingleCharacter []  = False
isSingleCharacter [c] = False
isSingleCharacter (c1:c2:str)
  | isApostrophe c1 || isSpace c1 || isApostrophe c2 == False = False
  | otherwise                                                  = True

giveSingleCharacter :: [Char] -> [Char]
giveSingleCharacter (c1:c2:str) = [c1] ++ [c2]

giveOctDigitSequence :: [Char] -> [Char]
giveOctDigitSequence [c]
  | isOctDigit c = [c]
  | otherwise = []
giveOctDigitSequence (c:str)
  | isOctDigit c = c : giveOctDigitSequence str
  | otherwise    = []

-- isOctalEscape only checks at most 3 characters that follow \, so the input string will have at most 3 characters.
isOctalEscape :: [Char] -> Bool
isOctalEscape [] = False
isOctalEscape [c]
  | isOctDigit c = True
  | otherwise    = False
isOctalEscape (c:str)
  | length (c:str) < 3 && isOctDigit c     = isOctalEscape str
  | length (c:str) == 3 && isZeroToThree c = isOctalEscape str
  | otherwise                              = False

-- Input for giveOctalEscape is already checked to be an OctalEscape token.
giveOctalEscape :: [Char] -> [Char]
giveOctalEscape [c]
  | isOctDigit c = [c]
  | otherwise    = []
giveOctalEscape (c:str)
  | len == 0 = []
  | len > 0 && len < 3 = c : giveOctalEscape str
  | len >= 3 && isZeroToThree c == False = giveOctalEscape (take 2 (c:str))
  | len >= 3 && isZeroToThree c = c : giveOctalEscape (take 2 (c:str))
  where len = length (giveOctDigitSequence (c:str))

isEscapeSequence :: [Char] -> Bool
isEscapeSequence []  = False
isEscapeSequence [c]
  | c `elem` ['b', 't', 'n', 'f', 'r', '"', '\'', '\\'] = True
  | otherwise = False
isEscapeSequence (c:str) = False

-- giveEscapeSequence includes OctalEscape.
giveEscapeSequence :: [Char] -> [Char]
giveEscapeSequence [c]
  | isOctDigit c || isEscapeSequence [c] = [c]
  | otherwise = []
giveEscapeSequence (c:str)
-- Only need to check for isOctalEscape here because isEscapeSequence should only contain 1 character.
  | isOctalEscape (c:str) = giveOctalEscape (c:str)
  | otherwise = []

isStringCharacter :: [Char] -> Bool
isStringCharacter [c]
  | isDoubleQuote c = False
  | otherwise = True
isStringCharacter (c:str) = isStringCharacter [c]

giveStringLiteral :: [Char] -> [Char]
giveStringLiteral [] = error "invalid token"
giveStringLiteral [c]
  | isStringCharacter [c] = [c]
  | otherwise = []
giveStringLiteral (c1:c2:str) =
  case (w, x, y, z) of
    (True, _, _, _) ->
      c1 : giveStringLiteral (c2:str)
    (_, True, True, False) ->
      (c1 : octEsc) ++ giveStringLiteral (cutString octEsc (c2:str))
    (_, True, False, True) -> c1 : c2 : giveStringLiteral str
    (_, True, False, False) -> []
    (_, False, _, _) -> []
  where
    w = isStringCharacter (c1:c2:str)
    x = isBackslash c1
    y = isOctDigit c2 
    z = isEscapeSequence [c2]
    octEsc = giveOctalEscape (c2:str)

isJavaSeparator :: [Char] -> Bool
isJavaSeparator [] = False
isJavaSeparator [c]
  | c `elem` ['(', ')', '{', '}', '[', ']', ';', ',', '.', '@'] = True
  | otherwise = False
isJavaSeparator (c:str)
  | take 2 (c:str) == "::" = True
  | take 3 (c:str) == "..." = True
  | otherwise = isJavaSeparator [c]

giveJavaSeparator :: [Char] -> [Char]
giveJavaSeparator [] = []
giveJavaSeparator [c]
  | isJavaSeparator [c] = [c]
  | otherwise = []
giveJavaSeparator (c:str)
  | take 3 (c:str) == "..." = take 3 (c:str)
  | take 2 (c:str) == "::" = take 2 (c:str)
  | otherwise = [c]

isJavaOperator :: [Char] -> Bool
isJavaOperator [] = False
isJavaOperator [c]
  | c `elem` ['=', '>', '<', '!', '~', '?', ':', '+', '-', '*', '/', '&', '|', '^', '%'] = True
  | otherwise = False
isJavaOperator (c:str)
  | take 2 (c:str) `elem` ["->", "==", ">=", "<=", "!=", "&&", "||", "++", "--", "<<", ">>", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%="] = True
  | take 3 (c:str) `elem` ["<<=",  ">>=", ">>>"] = True
  | take 4 (c:str) == ">>>=" = True
  | otherwise = isJavaOperator [c]

giveJavaOperator :: [Char] -> [Char]
giveJavaOperator [] = []
giveJavaOperator [c]
  | isJavaOperator [c] = [c]
  | otherwise = []
giveJavaOperator (c:str)
  | take 4 (c:str) == ">>>=" = take 4 (c:str)
  | take 3 (c:str) `elem` ["<<=",  ">>=", ">>>"] = take 3 (c:str)
  | take 2 (c:str) `elem` ["->", "==", ">=", "<=", "!=", "&&", "||", "++", "--", "<<", ">>", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%="] = take 2 (c:str)
  | otherwise = [c]
  
cutString :: String -> String -> String
cutString [_] [] = []
cutString substr str = drop (length substr) str

getSubstringBefore :: Char -> String -> String
getSubstringBefore ch str = takeWhile (/= ch) str

findMatching :: Char -> String -> Bool
findMatching ch str = ch `elem` str

lex1 :: Char -> String -> (String, String)
lex1 ch []
  | isJavaLetter ch || isDigit ch || isJavaSeparator [ch] || isJavaOperator [ch] = ([ch], [])
  | otherwise = ([], [])
lex1 ch (c:str)
  | isJavaLetter ch   =
    ([ch] ++ giveIdentifier (c:str), cutString (giveIdentifier (c:str)) (c:str))

  | isNonZeroDigit ch =
    ([ch] ++ giveDecimalNumeral (c:str), cutString (giveDecimalNumeral (c:str)) (c:str))
    
  | isZero ch =
    ([ch] ++ rest, cutString rest (c:str))
    
  | isDoubleQuote ch = case (a, b) of
      (False, _)    -> error "invalid token"
      (True, False) -> error "invalid token"
      (True, True)  ->
        ([ch] ++ giveStringLiteral(c:str) ++ [ch], cutString [ch] remainder)
      
  | isApostrophe ch =
    case (d, e, f, g) of
      (False, _, _, _)           ->
        error "invalid token"
      (True, False, True, _)     ->
        (ch : giveSingleCharacter (c:str), cutString (giveSingleCharacter (c:str)) (c:str))
      (True, True, False, True)  ->
        ([ch] ++ [c] ++ giveEscapeSequence fstSubstr ++ [ch], cutString (fstSubstr ++ [ch]) str)
      (True, True, True, _) ->
        (ch : giveSingleCharacter (c:str), cutString (giveSingleCharacter (c:str)) (c:str))
      (True, True, False, False) ->
        error "invalid token"
      (True, False, False, _)    ->
        error "invalid token"
  | isJavaSeparator (ch:c:str) = (separator, cutString separator (ch:c:str))
  | isJavaOperator (ch:c:str) = (operator, cutString operator (ch:c:str))
  
    where
      rest =
            case (w, x, y, z) of
              (_, True, False, False)      -> [c] ++ giveHexNumeral str
              (_, False, True, False)      -> [c] ++ giveBinaryNumeral str
              (_, False, False, True)      -> giveOctalNumeral (c:str)
              (True, _, _, _)              -> [c]
              (False, False, False, False) -> []
            where
              w = isIntegerTypeSuffix c
              x = isHexNumeral (c:str)
              y = isBinaryNumeral (c:str)
              z = isOctalNumeral (c:str)
              
      remainder = cutString (giveStringLiteral (c:str)) (c:str)
      a = findMatching ch (c:str)
      b = isDoubleQuote (head remainder)
      
      fstSubstr = getSubstringBefore ch str
      d = findMatching ch (c:str)
      e = isBackslash c
      f = isSingleCharacter (c:str)
      g = isOctalEscape fstSubstr || isEscapeSequence fstSubstr

      separator = giveJavaSeparator (ch:c:str)
      operator = giveJavaOperator (ch:c:str)

takeComment :: [Char] -> [Char]
takeComment [] = []
takeComment [c]
  | c == '\n' = []
  | otherwise = [c]
takeComment (c:str)
  | c == '\n' = str
  | otherwise = takeComment str

takeMultilineComment :: [Char] -> [Char]
takeMultilineComment [] = []
takeMultilineComment [c] = [c]
takeMultilineComment (c1:c2:str)
  | isStar c1 && isSlash c2 = str
  | otherwise = takeMultilineComment (c2:str)
  
findToken :: String -> String
findToken [] = []
findToken [c]
  | isSpace c = []
  | otherwise = [c]
findToken (c1:c2:str)
  | isSpace c1 = findToken (c2:str)
  | isSlash c1 && isStar c2 = findToken (takeMultilineComment str)
  | isSlash c1 && isSlash c2 = findToken (takeComment str)
  | otherwise = (c1:c2:str)

lexNoPrefix :: String -> [String]
lexNoPrefix [] = []
lexNoPrefix (c:str) = (fst (lex1 c str)) : lexJava (snd (lex1 c str))

lexJava :: String -> [String]
lexJava [] = []
lexJava str = lexNoPrefix (findToken str)
