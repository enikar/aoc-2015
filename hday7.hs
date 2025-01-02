-- AoC 2025, day 7, using direct reduction
-- It is 60 times faster than using the Haskell interpreter as
-- in hday7-hint.hs
-- There are 339 equations, we loop 208 times to get the solution.

-- | The input file consists of one equation per line.
-- Each equation is shaped as:
-- expresion -> var
--  - var is a sequence of 1 or 2 lower-case characters.
--  - expresion can be either:
--     - a term (either an integer or a var)
--     - the unary operator NOT follow by a term
--     - a binary operator one among "OR", "AND", "RSHIFT", "LSHIFT"
--       between to term.
--
-- Examples:
-- 1 AND fi -> fj    -- fj = 1 .&. fi
-- dy OR ej -> ek    -- ek = dy .|. ej
-- NOT jy -> jz      -- jz = complement jy
-- 44430 -> b        -- b = 44430
-- lx -> a           -- a = lx
-- b RSHIFT 3 -> e   -- e = b `shiftR` 3
--
-- The goal is to get the final value of the variable "a".
--
-- We build a list of equations, each equation is a pair (Term, Expr)
-- Here the Term == (Var string), in other words a left-value.
-- Expr is either:
--    - (AppNot term)
--    - (AppOp op term1 term2)
--    - AppSet term
-- where terms is either (Val n) or (Var string)
-- and op is Or, And, Rshift or Lshift
-- All of these equations are built from ReadP

{- HLINT ignore "Eta reduce" -}

module Main(main) where

import Data.Semigroup
  (Endo(..)
  ,stimes
  )

import Data.Word (Word16)
import Data.Maybe (fromMaybe)
import Data.Bits
  ((.|.)
  ,(.&.)
  ,complement
  ,shiftL
  ,shiftR
  )

-- module for parsing
import Data.Char
  (isDigit
  ,isUpper
  ,isLower
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
  ,pfail
  )

-- | Op are binary operators. Not is an unary operator,
-- it is handle separately
data Op = Or
          |And
          |Rshift
          |Lshift
           deriving (Show, Eq)

data Term = Var String
           |Val Word16
            deriving (Show, Eq)

data Expr = AppNot Term
            |AppOp Op Term Term
            |AppSet Term
             deriving (Show, Eq)

type Circuit = [(Term, Expr)]

main :: IO ()
main = do
  eqs <- parseDatas <$> readFile "day7.txt"
  let n = solve eqs
  printSolution "Part1" n
  printSolution "Part2" (solve (wireAonB n eqs))

-- | solve search for the value of "a".
-- Here getResult extract this value when found.
-- The function getResult is somewhat tricky.
--
-- But there is another way to write it. We filter the result
-- of iterate with (filter (not . null)) and take the last
-- elment of this list. Then we'll get the last non empty
-- Circuit which consists of 2 equations from our input.
-- But for now I can't get it to work properly.

solve :: Circuit -> Word16
solve eqs = getResult (iterate newCircuit eqs)
  where
    -- getResult is a recursive function to extract
    -- the result from the list of Circuit generate by
    -- iterate. It uses fromMaybe to exit or loop.
    -- Since iterate generates an infinte list and
    -- newCircuit reduces the length of Circuit,
    -- at some point the Circuit are [] and there is
    -- no solution.
    getResult :: [Circuit] -> Word16
    getResult ([]:_) = error "Error: solve: no solution"
    getResult (x:xs) = fromMaybe (getResult xs) (end x)
    getResult []     = error "Error: solve no solution" -- not reach

    -- Searches the equation (Var "a", AppSet (Val n)),
    -- if found returns (Just n) else returns Nothing.
    end :: Circuit -> Maybe Word16
    end [] = error "Error: solve: no solution"
    end eqs' = foldr h Nothing eqs'

    h (Var "a", AppSet (Val n)) _   = Just n
    h _                         acc = acc

-- | Computes the transiton for the state of Circuit
-- to the next state.
newCircuit :: Circuit -> Circuit
newCircuit eqs = foldr f eqs (findSubstition eqs)
  where
    f :: (Term, Word16) -> Circuit -> Circuit
    f (var, n) acc = substitute var n acc

-- | Finds all Expr equal to AppSet for which
-- the value of the left (Var x) is then knwon.
findSubstition :: Circuit -> [(Term, Word16)]
findSubstition eqs = foldr g [] eqs
  where
    g (var, AppSet (Val n)) acc  = (var, n) : acc
    g _                       acc = acc

-- | @substitute var n eqs@ subtstitutes var by the value n
-- in all equations eqs. Tries to reduce all Expr to (AppSet (val x))
-- applying AppNot and AppOp op in Expr when var is equal to a Term.
-- Removes the Term equal to (var, _).
-- Then that reduces the length of Circuit by one.
substitute :: Term -> Word16 -> Circuit -> Circuit
substitute (Val _) _ = error "Error: substitute cannot operate on a value"
substitute (Var x) n = foldr f []
  where
    f (Var y, _)                acc | x == y = acc
    f (var, AppSet (Var y))     acc | y == x = (var, AppSet (Val n)) : acc
    f (var, AppNot (Var y))     acc | y == x = (var, AppSet (Val (complement n))) : acc
    f (var, AppOp op (Var y) t) acc | y == x = (var, reduce op (Val n) t) : acc
    f (var, AppOp op t (Var y)) acc | y == x = (var, reduce op t (Val n)) : acc
    f eq                        acc          = eq:acc

-- | Tries to reduce op t1 t2 to the Expr (AppSet (Val x)) when
-- possible.
reduce :: Op -> Term -> Term -> Expr
reduce op t1 t2 = case (t1, t2) of
  (Var v, val) -> AppOp op (Var v) val
  (val, Var v) -> AppOp op val (Var v)
  (Val x, Val y) -> applyOp op x y
{-# INLINE reduce #-}

-- | Applies a binary operator.
applyOp :: Op -> Word16 -> Word16 -> Expr
applyOp op x y = AppSet . Val  $ case op of
  Or -> x .|. y
  And -> x .&. y
  Rshift -> x `shiftR` fromIntegral y
  Lshift -> x `shiftL` fromIntegral y
{-# INLINE applyOp #-}

-- | @wireAonB a eqs@ replaces the equation beginning
-- with "b =" with "b = a". That is:
-- "b = 44430;" becomes: "b = 3176;"
-- The other equations remain unchanged.
wireAonB :: Word16 -> Circuit -> Circuit
wireAonB a eqs = foldr f [] eqs
  where
    f (Var "b", _) acc = (Var "b", AppSet (Val a)) : acc
    f eq           acc = eq : acc

printSolution :: Show a => String -> a -> IO ()
printSolution part x = putStrLn (part <> ": " <> show x)

-- parsing stuff
-- | skips a newline
newLine :: ReadP ()
newLine = void (char '\n')

-- | skips one or more spaces
spaces :: ReadP ()
spaces = skipMany1 (char ' ')

-- | parseDatas turns the parser readDatas into a Circuit
parseDatas :: String -> Circuit
parseDatas str =
  case readP_to_S readDatas str of
    [(circuit, "")] -> circuit
    []  -> error "Error: parseDatas: can't parse."
    _   -> error "Error: parseDatas: there are more than one result."

-- | readDatas parses the input file, split it into lines
-- returns a parser equals a Circuit
readDatas :: ReadP Circuit
readDatas = do
  eqs <- sepBy1 readEq newLine
  optional newLine
  eof
  pure eqs

-- | readEq parses just an equation
readEq :: ReadP (Term, Expr)
readEq = do
  eq <- readNot <++ readBinary <++ readSet
  spaces
  void (string "->")
  spaces
  var <- readVar
  pure (var, eq)

-- | readNot parses the string "NOT" follow by a term
readNot :: ReadP Expr
readNot = do
  void (string "NOT")
  spaces
  AppNot <$> readTerm

-- | readBinary parses a term, then a binary operator
-- and finally another term
readBinary :: ReadP Expr
readBinary = do
  t1 <- readTerm
  spaces
  op <- munch1 isUpper
  spaces
  t2 <- readTerm
  let f op' = pure (AppOp op' t1 t2)
  maybe pfail f (lookup op ops)

-- | parses a Term an put it inside AppSet
readSet :: ReadP Expr
readSet = AppSet <$> readTerm

-- | readTerm parses a positive number or a variable name
readTerm :: ReadP Term
readTerm = readNumber <++ readVar

readNumber :: ReadP Term
readNumber = Val . read <$> munch1 isDigit

-- | readVar parses the var name
readVar :: ReadP Term
readVar = Var <$> munch1 isLower

-- | operator mapping
ops :: [(String, Op)]
ops =
  [("OR", Or)
  ,("AND", And)
  ,("RSHIFT", Rshift)
  ,("LSHIFT", Lshift)
  ]
