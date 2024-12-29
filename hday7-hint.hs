-- AoC 2015, day 7. Ugly solution using Haskell interpreter.
-- Quite slow.

-- | The input file consists of one equation per line.
--  Each equation is shaped as:
--  expresion -> var
--   - var is a sequence of 1 or 2 characters.
--   - expresion can be either:
--     - a term (either an integer or a var)
--     - the unary operator NOT follow by a term
--     - a binary operator one among "OR", "AND", "RSHIFT", "LSHIFT"
--       between to term.
--  Examples:
-- 1 AND fi -> fj    -- fj = 1 .&. fi
-- dy OR ej -> ek    -- ek = dy .|. ej
-- NOT jy -> jz      -- jz = complement jy
-- 44430 -> b        -- b = 44430
-- lx -> a           -- a = lx
-- b RSHIFT 3 -> e   -- e = b `shiftR` 3
--
-- But there is an minor issue. Some variables have the same name as
-- Haskell reserved words. Then we need to rename them.
-- We build the string to be evaluated by Haskell interpreter directly
-- from ReadP
{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.List
  (intercalate
  ,isPrefixOf
  )
import Data.Maybe (fromMaybe)
import Data.Word (Word16)
import Language.Haskell.Interpreter

-- module for parsing
import Data.Char
  (isDigit
  ,isSpace
  ,isUpper
  )

import Control.Monad (void)
import Text.ParserCombinators.ReadP
  (ReadP
  ,(<++)
  ,readP_to_S
  ,char
  ,string
  ,sepBy1
  ,munch1
  ,skipMany1
  ,optional
  ,eof
  )

main :: IO ()
main = do
  equations <- parseDatas <$> readFile "day7.txt"
  n <- solve equations
  printSolution "Part1" n
  let equations' = wireAonB n equations
  n' <- solve equations'
  printSolution "Part2" n'

-- | solve str calls the haskell interpreter and manages
--  the enventually errors
solve :: String -> IO Word16
solve str = runInterpreter (evalString str)
                  >>= either errorSolve (pure . read)
  where
    errorSolve err = case err of
      (WontCompile es)  -> error (errP1 <> ws es)
      (UnknownError es) -> error (errP1 <> us es)
      (NotAllowed es)   -> error (errP1 <> ns es)
      (GhcException es) -> error (errP1 <> gs es)
      where
        errP1 = "Error: solve: "
        ws es = intercalate "\n" (wh : map unbox es)
        wh = "Won't compile:"
        unbox (GhcError e) = e
        us es = "Unknown Error: " <> es
        ns es = "Not allowed: " <> es
        gs es = "Ghc Exception: " <> es

-- | evalString evaluates str into a Haskell Interpreter
-- after importing module use for code in str.
evalString :: String -> Interpreter String
evalString str =
  setImportsQ [("Prelude", Nothing)
              ,("Data.Bits", Nothing)
              ,("Data.Word", Nothing)
              ] >>
  eval str

-- | wireAonB a eqs replace the equation for "b ="
--  to set b to the value of a. That is:
--  "b = 44430;" becomes: "b = 3176;"
wireAonB :: Word16 -> String -> String
wireAonB a str = unlines (foldr f [] (lines str))
  where
    f s acc = s' : acc
      where
        s' = if "b =" `isPrefixOf` s
             then "b = " <> show a <> ";"
             else s

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- parsing stuff

-- parses a newline
newLine :: ReadP ()
newLine = void (char '\n')

-- parses one or more spaces
spaces :: ReadP ()
spaces = skipMany1 (char ' ')

-- parses one or more character different from space characters,
-- especially different from ' ' and '\n'. This is to read variable
-- names
notSpace :: ReadP String
notSpace = munch1 (not . isSpace)

-- | parseDatas turns the parser readDatas into a string
parseDatas :: String -> String
parseDatas str =
  case readP_to_S readDatas str of
    [(circuit, "")] -> circuit
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

-- | readDatas parses the input file, split it into lines
-- returns a parser equals to the string build by buildCircuit
readDatas :: ReadP String
readDatas = do
  eqs <- sepBy1 readEq newLine
  optional newLine -- some files end with a newline some not…
  eof
  pure (buildCircuit eqs)

-- | readEq parses just an equation
readEq :: ReadP String
readEq = do
  eq <- readNot <++ readBinary <++ readTerm
  spaces
  void (string "->")
  spaces
  var <- readVar
  pure (var <> " = " <> eq)

-- | readNot parses the string "NOT" follow by a term
readNot :: ReadP String
readNot = do
  void (string "NOT")
  spaces
  t <- readTerm
  pure ("complement " <> t)

-- | readBinary parses a term, then a binary operator
-- and finally another term
readBinary :: ReadP String
readBinary = do
  t1 <- readTerm
  spaces
  op <- munch1 isUpper
  spaces
  t2 <- readTerm
  let op' = fromMaybe errorRead (lookup op ops)
      errorRead = error ("Error: readBinary: Not a known operator: " <> op)
  pure (t1 <> op' <> t2)

-- | readTerm read a positive number or a variable name
readTerm :: ReadP String
readTerm = munch1 isDigit <++ readVar

-- | readVar read the var name and rename it if needed
readVar :: ReadP String
readVar = do
  var <- notSpace
  pure (case var of
          "do" -> "do'"
          "if" -> "if'"
          "in" -> "in'"
          "of" -> "of'" -- not in the input…
          "or" -> "or'" -- neither this one
          "id" -> "id'" -- not a reserverd word, but it doesn't hurt
          _    -> var)

-- | buildCircuit strs is equal to the string to be evaluated
buildCircuit :: [String] -> String
buildCircuit strs = "let { "
                    <> intercalate ";\n" strs
                    <> " } in a :: Word16"

-- | operator mapping
ops :: [(String, String)]
ops = [("OR", " .|. ")
      ,("AND", " .&. ")
      ,("RSHIFT", " `shiftR` ")
      ,("LSHIFT", " `shiftL` ")
      ]
