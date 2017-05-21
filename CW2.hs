module CW2 where

import Prelude hiding (Num)
import qualified Prelude (Num)

import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.String
import Text.Megaparsec.Expr

import qualified Text.Megaparsec.Lexer as L

import Data.Maybe
import Data.List

------------------------------------------------------------------
-----------------------------PART 1-------------------------------
------------------------------------------------------------------

-- Parse end of line
cr :: Parser String
cr = many (oneOf "\r\n")

-- Parse any string
tok :: String -> Parser String
tok t = whitespace *> string t <* whitespace

-- Parse whitespace
ws :: Parser ()
ws = oneOf " \t\n\r" *> pure ()

-- Parse comments
whitespace :: Parser ()
whitespace = L.space ws lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"

-- MultiLine Comment
mlComment :: Parser ()
mlComment = tok "/*" *> manyTill anyChar (try (tok "*/")) *> pure ()

-- Ensures parentheses are enclosing the input
parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

-- The following functions parse single chars that are often used in
-- the language. the function eats whitespace to the left and right
-- of the char.

asterisk :: Parser Char
asterisk = whitespace *> char '*' <* whitespace

plus :: Parser Char
plus = whitespace *> char '+' <* whitespace

minus :: Parser Char
minus = whitespace *> char '-' <* whitespace

myNot :: Parser Char
myNot = whitespace *> char '!' <* whitespace

myAnd :: Parser Char
myAnd = whitespace *> char '&' <* whitespace

equals :: Parser Char
equals = whitespace *> char '=' <* whitespace

lessOrEqual :: Parser String
lessOrEqual = whitespace *> tok "<=" <* whitespace

assign :: Parser String
assign = whitespace *> tok ":=" <* whitespace

semicolon :: Parser Char
semicolon = whitespace *> char ';' <* whitespace

--------------------------------------------------------------------------------

-- List of words that are reserved by the language
reserved :: [String]
reserved = ["skip", "if", "then", "else", "while", "begin", "end", "call", "var", "proc", "is", "TRUE", "FALSE"]

------------------------------------------------------------------------------

type Num   = Integer
type Var   = String
type Pname = String
type DecV  = [(Var, Aexp)]
type DecP  = [(Pname, Stm)]

-- Parses an integer
parseNum :: Parser Num
parseNum = (pure read <*> some (oneOf "0123456789")) <* whitespace

-- Parses a string, includes underscore
parseVar :: Parser Var
parseVar = whitespace *> some (oneOf (['A'..'Z'] ++ ['a'..'z'] ++ ['_'])) <* whitespace

-- Checks that a Var identifier is not a reserved keyword and applies parseVar
-- if it isn't
parseVarIdent :: Parser Var
parseVarIdent = try (p >>= check)
  where
    p       = parseVar
    check x
      | x `elem` reserved = fail $ "keyword " ++ show x ++ "cannot be an identifier"
      | otherwise         = return x

-- Parses a DecV into the specified data type
parseDecV :: Parser DecV
parseDecV = many ((,) <$ tok "var" <*> parseVarIdent <* assign <*> parseAexp <* semicolon)

-- Parses a DecP into the specified data type
parseDecP :: Parser DecP
parseDecP = many ((,) <$ tok "proc" <*> parsePnameIdent <* tok "is" <*> parseSterm  <* semicolon)

-- Parses a Pname
parsePname :: Parser Pname
parsePname = whitespace *> some (oneOf (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['_'])) <* whitespace

-- Checks that a Pname identifier is not a reserved keyword and applies
-- parsePname if it isn't
parsePnameIdent :: Parser Pname
parsePnameIdent = try (p >>= check)
  where
    p       = parsePname
    check x
      | x `elem` reserved = fail $ "keyword " ++ show x ++ "cannot be an identifier"
      | otherwise         = return x

--------------------------------------------------------------------------------

-- AST for an arithmetic expression
data Aexp = N Num
          | V Var
          | Mult Aexp Aexp
          | Add Aexp Aexp
          | Sub Aexp Aexp
          deriving (Show)

-- Here we make use of the makeExprParser function from magaparsec. Given the
-- terms, and operations of an expression. It builds a parser for said
-- expression.
parseAexp :: Parser Aexp
parseAexp = makeExprParser parseAterm parseAops <* whitespace

parseAterm :: Parser Aexp
parseAterm =  parens parseAexp
          <|> N <$> parseNum
          <|> V <$> parseVarIdent

parseAops :: [[Operator Parser Aexp]]
parseAops = [ [ InfixL (Mult <$ asterisk) ]
            , [ InfixL (Add  <$ plus)
              , InfixL (Sub  <$ minus)    ]
            ]

--------------------------------------------------------------------------------

-- AST for a boolean expression
data Bexp = TRUE
          | FALSE
          | Neg Bexp
          | And Bexp Bexp
          | Le Aexp Aexp
          | Eq Aexp Aexp
          deriving (Show)


-- Again we use the makeExprParser function
parseBexp :: Parser Bexp
parseBexp = makeExprParser parseBterm parseBops <* whitespace

parseBterm :: Parser Bexp
parseBterm =  parens parseBexp
          <|> try (Le <$> parseAexp <* lessOrEqual <*> parseAexp)
          <|> try (Eq <$> parseAexp <* equals      <*> parseAexp)
          <|> TRUE  <$ tok "TRUE"
          <|> FALSE <$ tok "FALSE"



parseBops :: [[Operator Parser Bexp]]
parseBops = [ [ Prefix (Neg <$ myNot) ]
            , [ InfixL (And <$ myAnd) ]
            ]

-- Since '<=' and '=' take arguments that are not Bexps, they must be parsed by
-- hand.
parseRexp :: Parser Bexp
parseRexp =  Le <$> try (parseAexp <* lessOrEqual) <*> parseAexp
         <|> Eq <$> try (parseAexp <* equals)      <*> parseAexp

--------------------------------------------------------------------------------

-- AST for a statement
data Stm   = Skip
           | Ass Var Aexp
           | Comp Stm Stm
           | If Bexp Stm Stm
           | While Bexp Stm
           | Block DecV DecP Stm
           | Call Pname
           deriving (Show)

parseSterm :: Parser Stm
parseSterm =  parens parseStm
       <|> try (Ass   <$> parseVarIdent <*  tok ":="         <*> parseAexp)
       <|> try (Call  <$  tok "call"    <*> parsePnameIdent)
       <|> try (If    <$  tok "if"      <*> parseBexp        <* tok "then" <*> parseStm <* tok "else" <*> parseStm)
       <|> try (While <$  tok "while"   <*> parseBexp        <* tok "do"   <*> parseStm)
       <|> try (Skip  <$  tok "skip")
       <|> try (Block <$  tok "begin"   <*> parseDecV        <*> parseDecP <*> parseStm <* tok "end" )

parseSops :: [[Operator Parser Stm]]
parseSops = [[ InfixR (Comp  <$ tok ";")    ]]

parseStm :: Parser Stm
parseStm = makeExprParser (whitespace *> parseSterm) parseSops

--------------------------------------------------------------------------------

parse :: String -> Stm
parse str = fromMaybe Skip (parseMaybe (whitespace *> parseStm) str)

--------------------------------------------------------------------------------
-------------------------------------PART 2-------------------------------------
--------------------------------------------------------------------------------

type T       = Bool
type Z       = Integer
type State = Var -> Z

-- Evaluates an arithmetic expression given a state
aExpEval :: Aexp -> State -> Z
aExpEval (N n) s       = n
aExpEval (V v) s       = s v
aExpEval (Mult a a') s = (aExpEval a s) * (aExpEval a' s)
aExpEval (Add a a') s  = (aExpEval a s) + (aExpEval a' s)
aExpEval (Sub a a') s  = (aExpEval a s) - (aExpEval a' s)

-- Evaluates an boolean expression given a state
bExpEval :: Bexp -> State -> T
bExpEval (TRUE) s     = True
bExpEval (FALSE) s    = False
bExpEval (Neg b) s    = not (bExpEval b s)
bExpEval (And b b') s = (bExpEval b s) && (bExpEval b' s)
bExpEval (Le a a') s  = (aExpEval a s) <= (aExpEval a' s)
bExpEval (Eq a a') s  = (aExpEval a s) == (aExpEval a' s)

--------------------------------------------------------------------------------
-----------------------------DYNAMIC SCOPE--------------------------------------
--------------------------------------------------------------------------------

type DEnvP = Pname -> Stm

-- Adds a variable v and its assignment z to a state s, creating a new state s'.
-- if s' is called on v, it will return z, otherwise it will return what s would
-- have
updateState :: State -> Z -> Var -> State
updateState s z v = newState where
                  newState x
                    | x == v    = z
                    | otherwise = s x


resetState :: State -> State -> DecV -> State
resetState s s' ((v, a) : decs) = resetState s (updateState s' (s v) v) decs
resetState s s' []              = s'

d_decVhelper :: DecV -> State -> State
d_decVhelper [] s         = s
d_decVhelper ((v, a) :decs) s = d_decVhelper decs (updateState s (aExpEval a s) v)

d_updateEnvP :: DecP -> DEnvP -> DEnvP
d_updateEnvP []                envP = envP
d_updateEnvP ((p, stm) : decs) envP = d_updateEnvP decs newEnv
  where newEnv x
          | x == p    = stm
          | otherwise = envP x

sDynamic :: DEnvP -> Stm -> State -> State
sDynamic envP (Skip) s            = s
sDynamic envP (Ass v a) s         = updateState s (aExpEval a s) v
sDynamic envP (Comp stm stm') s   = sDynamic envP stm' (sDynamic envP stm s)
sDynamic envP (If b stm stm') s
  | bExpEval b s                  = sDynamic envP stm  s
  | otherwise                     = sDynamic envP stm' s
sDynamic envP (While b stm) s
  | bExpEval b s                  = sDynamic envP (While b stm) (sDynamic envP stm s)
  | otherwise                     = s
sDynamic envP (Block dv dp stm) s = resetState s (sDynamic (d_updateEnvP dp envP) stm (d_decVhelper dv s)) dv
sDynamic envP (Call p) s          = sDynamic envP (envP p) s

s_dynamic :: Stm -> State -> State
s_dynamic stm s = sDynamic d_initialEnvP stm s

--------------------------------------------------------------------------------
-----------------------------DYNAMIC TESTING------------------------------------
--------------------------------------------------------------------------------

d_initialEnvP :: DEnvP
d_initialEnvP = \_ -> error "oops"

--------------------------------------------------------------------------------
------------------------------MIXED SCOPE---------------------------------------
--------------------------------------------------------------------------------

newtype MEnvP  = MEnvP (Pname -> (Stm, MEnvP, DecP))

m_updateEnvP :: DecP -> MEnvP -> DecP -> MEnvP
m_updateEnvP []               envP         dp = envP
m_updateEnvP ((p,stm) : decs) (MEnvP envP) dp = m_updateEnvP decs (MEnvP newEnv) dp
  where newEnv x
          | x == p    = (stm, MEnvP envP, dp)
          | otherwise = envP x

sMixed :: MEnvP -> Stm -> State -> State
sMixed envP (Skip) s            = s
sMixed envP (Ass v a) s         = updateState s (aExpEval a s) v
sMixed envP (Comp stm stm') s   = sMixed envP stm' (sMixed envP stm s)
sMixed envP (If b stm stm') s
  | bExpEval b s                = sMixed envP stm  s
  | otherwise                   = sMixed envP stm' s
sMixed envP (While b stm) s
  | bExpEval b s                = sMixed envP (While b stm) (sMixed envP stm s)
  | otherwise                   = sMixed envP Skip s
sMixed envP (Block dv dp stm) s = resetState s (sMixed envP' stm s') dv
  where envP' = m_updateEnvP dp envP dp
        s'    = d_decVhelper dv s
sMixed (MEnvP envP) (Call p) s  = sMixed envP'' stm s
  where (stm, envP', dp) = envP p
        envP''           = m_updateEnvP dp envP' dp

s_mixed :: Stm -> State -> State
s_mixed stm s = sMixed m_initialEnvP stm s

--------------------------------------------------------------------------------
---------------------------------MIXED TESTING----------------------------------
--------------------------------------------------------------------------------

m_initialEnvP :: MEnvP
m_initialEnvP = MEnvP (\_ -> error "oops")

--------------------------------------------------------------------------------
------------------------------STATIC SCOPE--------------------------------------
--------------------------------------------------------------------------------

type Loc    = Z

type Store  = Loc -> Z
type SEnvV  = Var -> Loc

newtype SEnvP  = SEnvP (Pname -> (Stm, SEnvV, SEnvP, DecP))

next :: Loc
next = -1

new :: Loc -> Loc
new x = x + 1

s_updateEnvV :: DecV -> SEnvV -> Store -> (SEnvV, Store)
s_updateEnvV [] envV sto             = (envV, sto)
s_updateEnvV ((v,a) : decs) envV sto = s_updateEnvV decs newEnv newSto
  where newEnv x
          | x == v    = sto next
          | otherwise = envV x
        newSto x
          | x == sto next = aExpEval a (sto . envV)
          | x == next     = new (sto next)
          | otherwise     = sto x

s_updateEnvP :: DecP -> SEnvP -> SEnvV -> DecP -> SEnvP
s_updateEnvP []                envP        envV dp = envP
s_updateEnvP ((p,stm) : decs) (SEnvP envP) envV dp = s_updateEnvP decs (SEnvP newEnvP) envV dp
  where newEnvP x
          | x == p    = (stm, envV, SEnvP envP, dp)
          | otherwise = envP x

updateSto :: SEnvV -> Store -> Var -> Z -> Store
updateSto envV sto v a = sto'
  where sto' x
          | x == envV v = a
          | otherwise   = sto x


sStatic :: SEnvV -> SEnvP -> Stm -> Store -> Store
sStatic envV envP (Skip)            sto = sto
sStatic envV envP (Ass v a)         sto = updateSto envV sto v (aExpEval a (sto . envV))
sStatic envV envP (Comp stm stm')   sto = sStatic envV envP stm' sto'
  where sto' = (sStatic envV envP stm sto)
sStatic envV envP (If b stm stm')   sto
  | bExpEval b (sto . envV)             = sStatic envV envP stm  sto
  | otherwise                           = sStatic envV envP stm' sto
sStatic envV envP (While b stm)     sto
  | bExpEval b (sto . envV)             = sStatic envV envP (While b stm) (sStatic envV envP stm sto)
  | otherwise                           = sto
sStatic envV envP (Block dv dp stm) sto = sStatic envV' envP' stm sto'
  where (envV', sto') = s_updateEnvV dv envV sto
        envP'         = s_updateEnvP dp envP envV dp
sStatic envV (SEnvP envP) (Call p)  sto = sStatic envV' envP'' stm sto
  where (stm, envV', envP', dp) = envP p
        envP''                  = s_updateEnvP dp envP' envV' dp

s_dv_findVars :: DecV -> [Var]
s_dv_findVars []              = []
s_dv_findVars ((v, a) : decs) = (v : aExp_findVars a) ++ s_dv_findVars decs

s_dp_findVars :: DecP -> [Var]
s_dp_findVars []                = []
s_dp_findVars ((p, stm) : decs) = s_findVars stm ++ s_dp_findVars decs

aExp_findVars :: Aexp -> [Var]
aExp_findVars (N n)       = []
aExp_findVars (V v)       = [v]
aExp_findVars (Mult a a') = (aExp_findVars a) ++ (aExp_findVars a')
aExp_findVars (Add a a')  = (aExp_findVars a) ++ (aExp_findVars a')
aExp_findVars (Sub a a')  = (aExp_findVars a) ++ (aExp_findVars a')

bexp_findVars :: Bexp -> [Var]
bexp_findVars (TRUE)     = []
bexp_findVars (FALSE)    = []
bexp_findVars (Neg b)    = bexp_findVars b
bexp_findVars (And b b') = (bexp_findVars b) ++ (bexp_findVars b')
bexp_findVars (Le a a')  = (aExp_findVars a) ++ (aExp_findVars a')
bexp_findVars (Eq a a')  = (aExp_findVars a) ++ (aExp_findVars a')

s_findVars :: Stm -> [Var]
s_findVars (Skip)            = []
s_findVars (Ass v a)         = [v]
s_findVars (Comp stm stm')   = s_findVars stm ++ s_findVars stm'
s_findVars (If b stm stm')   = bexp_findVars b ++ s_findVars stm ++ s_findVars stm'
s_findVars (While b stm)     = bexp_findVars b ++ s_findVars stm
s_findVars (Block dv dp stm) = s_findVars stm ++ s_dv_findVars dv ++ s_dp_findVars dp
s_findVars (Call p)          = []

s_initialise :: [Var] -> State -> SEnvV -> Store -> (SEnvV, Store)
s_initialise []     s envV sto = (envV, sto)
s_initialise (v:vs) s envV sto = s_initialise vs s newEnvV newSto
  where newEnvV x
          | x == v    = sto next
          | otherwise = envV x
        newSto x
          | x == sto next = s v
          | x == next     = new (sto next)
          | otherwise     = sto x

s_static :: Stm -> State -> State
s_static stm s = (sStatic envV s_initialEnvP stm sto) . envV
  where (envV, sto) = s_initialise (nub (s_findVars stm)) s s_initialEnvV s_initialStore

------------------------------------------------------------------------------------
-----------------------------------STATIC TESTING-----------------------------------
------------------------------------------------------------------------------------

s_initialEnvV :: SEnvV
s_initialEnvV  _  = 0

s_initialStore :: Store
s_initialStore x
  | x == next = 1
  | otherwise = 0

s_initialEnvP :: SEnvP
s_initialEnvP = SEnvP (\_ -> error "oops")
