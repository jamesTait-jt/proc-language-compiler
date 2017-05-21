--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------------------------- Part 2 --------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO list --
--------------------------------------------------------------------------------
--TODO: Fix/Test if self recursion works for static
--TODO: Fix Mutual recursion for static
--TODO: Fix Mutual recursion for mixed

-- Imports --
--------------------------------------------------------------------------------

import Prelude hiding (Num)
import qualified Prelude (Num)
import Text.Megaparsec hiding (State)
import Text.Megaparsec.String
import Text.Megaparsec.Expr
import Data.Maybe
import Data.List
import qualified Text.Megaparsec.Lexer as L


-- BNF --
--------------------------------------------------------------------------------

--  S ::= x := a| skip | S1 ;S2 | if b then S1 else S2 | while b do S | begin DV DP S end | call p
--  DV ::= var x := a ; DV |ε
--  DP ::= proc p is S; DP |ε


-- Testing --
--------------------------------------------------------------------------------

parseTester :: Parser Stm
parseTester = parseStm

test :: String -> String
test s =
  case ret of
    Left err -> "error: " ++ parseErrorPretty err
    Right n -> show n
  where
    ret = parse parseTester "" s

main :: IO ()
main = interact (unlines . map test . lines)

--parseFile :: FilePath -> IO ()
--parseFile filePath = do
  --file <- readFile filePath
  --putStrLn $ case parse parseProg filePath file of
    --Left err        -> parseErrorPretty err
    --Right parseProg -> show parseProg

data Program = Program [Stm]
              deriving (Show)

parseProg :: Parser Program
parseProg = Program <$ whitespace <*> many parseStm


myParse :: String -> Stm
myParse str = fromMaybe Skip (parseMaybe (whitespace *> parseStm) str)


-- General Parsing --
--------------------------------------------------------------------------------

cr :: Parser String
cr = many (oneOf "\r\n")

tok :: String -> Parser String
tok t = whitespace *> string t <* whitespace

ws :: Parser ()
ws = oneOf " \t\n" *> pure ()

whitespace :: Parser ()
whitespace = L.space ws lineComment blockComment
  where
    lineComment  = L.skipLineComment "//"
    blockComment = L.skipBlockComment "/*" "*/"


-- Symbols --
--------------------------------------------------------------------------------

parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

star :: Parser Char
star = whitespace *> char '*' <* whitespace

plus :: Parser Char
plus = whitespace *> char '+' <* whitespace

minus :: Parser Char
minus = whitespace *> char '-' <* whitespace

negation :: Parser Char
negation = whitespace *> char '!' <* whitespace

logAnd :: Parser Char
logAnd = whitespace *> char '&' <* whitespace

equals :: Parser Char
equals = whitespace *> char '=' <* whitespace

lessEq :: Parser String
lessEq = whitespace *> tok "<=" <* whitespace

assign :: Parser String
assign = whitespace *> tok ":=" <* whitespace

semicolon :: Parser Char
semicolon = whitespace *> char ';' <* whitespace


-- Keywords --
--------------------------------------------------------------------------------

keywords :: [String]
keywords = ["skip", "if", "then", "else", "while", "begin", "end", "call", "var", "proc", "is", "true", "false"]


-- Data Types --
--------------------------------------------------------------------------------

type Num   = Integer
type Var   = String
type Pname = String
type DecV  = [(Var,Aexp)]
type DecP  = [(Pname,Stm)]

data Aexp = N Num | V Var | Mult Aexp Aexp | Add Aexp Aexp | Sub Aexp Aexp deriving (Show)

data Bexp  = TRUE | FALSE | Neg Bexp | And Bexp Bexp
           | Le Aexp Aexp | Eq Aexp Aexp deriving (Show)

data Stm = Skip | Ass Var Aexp | Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm
          | Block DecV DecP Stm | Call Pname deriving (Show)


-- Aexp Parser --
--------------------------------------------------------------------------------

parseNum :: Parser Num
parseNum = (pure read <*> some (oneOf "0123456789")) <* whitespace

parseVarDo :: Parser Var
parseVarDo = whitespace *> some (oneOf (['a'..'z'] ++ ['_'])) <* whitespace

parseVar :: Parser Var
parseVar = try (p >>= check)
  where
    p       = parseVarDo
    check x
      | elem x keywords = fail $ "keyword " ++ show x ++ "cannot be used"
      | otherwise = return x

parseAexp :: Parser Aexp
parseAexp = makeExprParser parseAexpTerm parseAexpOp <* whitespace

parseAexpTerm :: Parser Aexp
parseAexpTerm =  parens parseAexp
          <|> N <$> parseNum
          <|> V <$> parseVar

parseAexpOp :: [[Operator Parser Aexp]]
parseAexpOp = [ [ InfixL (Mult <$ star)]
            , [ InfixL (Add  <$ plus)
              , InfixL (Sub  <$ minus)]]


-- Bexp Parser
--------------------------------------------------------------------------------

parseBexp :: Parser Bexp
parseBexp = makeExprParser parseBexpTerm parseBexpOp <* whitespace <* cr

parseBexpTerm :: Parser Bexp
parseBexpTerm =  parens parseBexp
          <|> try (Le <$> parseAexp <* lessEq <*> parseAexp)
          <|> try (Eq <$> parseAexp <* equals <*> parseAexp)
          <|> TRUE  <$ tok "true"
          <|> FALSE <$ tok "false"
          <|> parseRexp

parseBexpOp :: [[Operator Parser Bexp]]
parseBexpOp = [ [ Prefix (Neg <$ negation)]
            , [ InfixL (And <$ logAnd)]]


parseRexp :: Parser Bexp
parseRexp =  Le <$> try (parseAexp <* lessEq) <*> parseAexp
         <|> Eq <$> try (parseAexp <* equals) <*> parseAexp


-- Stm Parser --
--------------------------------------------------------------------------------

parseStm :: Parser Stm
parseStm = parens parseStm
  <|> do
  stms <- sepBy1 (whitespace *> parseAStm) semicolon -- assign Statements vy parsing 1 at a time seperated by semicolon
  if length stms == 1
    then return $ head stms
    else return $ foldr1 Comp stms


parseAStm :: Parser Stm
parseAStm =   parens parseAStm
              <|> Skip <$ tok "skip"
              <|> Ass <$> parseVar <* assign <*> parseAexp
              <|> If <$ tok "if" <*> parseBexp <* tok "then" <*> parseStm <* tok "else" <*> parseStm
              <|> While <$ tok "while" <*> parseBexp <* tok "do" <*> parens parseStm
              <|> Block <$ tok "begin" <*> parseDecV <*> parseDecP <*> parseStm <* tok "end"
              <|> Call  <$ tok "call" <*> parsePname

parseDecV :: Parser DecV
parseDecV = many ((,) <$ tok "var" <*> parseVar <* assign <*> parseAexp <* semicolon) <* cr

parseDecP :: Parser DecP
parseDecP = many ((,) <$ tok "proc" <*> parsePname <* tok "is" <*> parseAStm <* semicolon) <* cr

parsePnameDo :: Parser Pname
parsePnameDo = whitespace *> some (oneOf (['a' .. 'z'] ++ ['_'])) <* whitespace

parsePname :: Parser Pname
parsePname = try (p >>= check)
  where
    p = parsePnameDo
    check x
      | elem x keywords = fail $ "keyword " ++ show x ++ "cannot be used"
      | otherwise = return x


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
---------------------------------- Part 3 --------------------------------------
--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Stm Natural Semantics (Proc Language Dynamic Variables and Procedures)
--------------------------------------------------------------------------------

                          -- Type Definitions --

type T       = Bool
type Z       = Integer
type State = Var -> Z
type Env     = Pname -> Stm         --Dynamic Environment


        -- Updates a states variable to the specfied interger value  --

update :: State -> Z -> Var -> State
update myState i v =  ns where
                  ns x
                   | x == v = i
                   | otherwise = myState x


              -- Implements the semantics of a conditional choice --

cond :: (a->T, a->a, a->a) -> (a->a)
cond (b,opA,opB) s
 | b s  = opA s
 | otherwise  = opB s


                    -- Implements fix points for states --

fix :: (( State -> State ) -> ( State -> State )) -> ( State -> State )
fix ff = ff (fix ff)


                        -- Num Natural Semantics --

n_val :: Num -> Z
n_val num = num


                        -- Aexp Natural Semantics --

a_val :: Aexp -> State -> Z
a_val (N n) s       = n_val n
a_val (V v) s       = s v
a_val (Mult a a') s = (a_val a s) * (a_val a' s)
a_val (Add a a') s  = (a_val a s) + (a_val a' s)
a_val (Sub a a') s  = (a_val a s) - (a_val a' s)


                        -- Bexp Natural Semantics --

b_val :: Bexp -> State -> T
b_val (TRUE) s      = True
b_val (FALSE) s     = False
b_val (Neg b) s     = not (b_val b s)
b_val (And b b') s  = (b_val b s) && (b_val b' s)
b_val (Eq a a') s   = (a_val a s) == (a_val a' s)
b_val (Le a a') s   = (a_val a s) <  (a_val a' s)


              -- Stm Natural Semantics (Basic While Language) --

s_while :: Stm -> State -> State
s_while (Ass v a) s = update s (a_val a s) v
s_while (Skip) s = s
s_while (Comp stm1 stm2) s = s_while stm2 (s_while stm1 s)
s_while (If b stm1 stm2) s = cond (b_val b, s_while stm1, s_while stm2) s
s_while (While b stm) s    = fix f s where
                          f g = cond((b_val b),(g.(s_while stm)),s_while Skip)


  -- Stm Natural Semantics (Proc Language Dynamic Variables and Procedures) --

s_dynmaic_ev :: Stm -> Env -> State -> State
s_dynmaic_ev (Ass v a) e s = update s (a_val a s) v
s_dynmaic_ev (Skip) e s = s
s_dynmaic_ev (Comp stm1 stm2) e s = s_dynmaic_ev stm2 e (s_dynmaic_ev stm1 e s)
s_dynmaic_ev (If b stm1 stm2) e s = cond (b_val b, s_dynmaic_ev stm1 e, s_dynmaic_ev stm2 e) s
s_dynmaic_ev (While b stm) e s = fix f s where
                        f g = cond( (b_val b),(g.(s_dynmaic_ev stm e)),s_dynmaic_ev Skip e)
s_dynmaic_ev (Block v p stm) e s = reset_state s (s_dynmaic_ev stm (eval_decP p e) (eval_decV v s)) v
s_dynmaic_ev (Call name) e s = s_dynmaic_ev (e name) e s


                      -- Evaluates DecV --

eval_decV :: DecV -> State -> State
eval_decV [] s = s
eval_decV (x:xs) s = eval_decV xs (update s (a_val (snd x) s) (fst x))


                      -- Evaluates DecP --

eval_decP :: DecP -> Env -> Env
eval_decP [] e = e
eval_decP (x:xs) e = eval_decP xs (update_env e (snd x) (fst x))


      -- Resets the current state back to what it was originally--

reset_state :: State -> State -> DecV -> State
reset_state ini_s fin_s decV = map_state fin_s decV where
  map_state :: State -> DecV -> State
  map_state ns [] = ns
  map_state ns (x:xs) = map_state (restore_state x ns) xs where
    restore_state :: (Var,Aexp) -> State -> State
    restore_state (x,a) ns = fs where
      fs v
        | v == x = (ini_s v)
        | otherwise = (ns v)


              -- Updates the procedure environment --

update_env :: Env -> Stm -> Pname -> Env
update_env env stm name =  ns where
                      ns x
                        | x == name = stm
                        | otherwise = env x


        -- Resets the procedure environment to original --

reset_env :: Env -> Env -> DecP -> Env
reset_env ini_env fin_env decP = map_env fin_env decP where
  map_env :: Env -> DecP -> Env
  map_env ns [] = ns
  map_env ns (x:xs) = map_env (restore_env x ns) xs where
    restore_env :: (Pname,Stm) -> Env -> Env
    restore_env (x,a) ns = fs where
      fs v
        | v == x = (ini_env v)
        | otherwise = (ns v)


-- Dynamic Wrapper Functions --
--------------------------------------------------------------------------------
env :: Env
env "hello" = Skip


s_dynmaic :: Stm -> State -> State
s_dynmaic stm s = s_dynmaic_ev stm env s


-- Dynamic Testing Functions --
--------------------------------------------------------------------------------

--Test State
state_dynamic :: State
state_dynamic "x" = 3
state_dynamic "y" = 4

scopeTest :: State -> Z
scopeTest s = (s_dynmaic (myParse "begin var x:=0; proc p is (x:=x*2); proc q is (call p); begin var x:=5; proc p is (x:=x+1); call q; y:=x end end") s) "y"

facdyn :: State -> Z
facdyn s = (s_dynmaic (myParse "begin proc fac is (begin var z:=x; if (x=1) then (skip) else (x:=x-1; call fac; y:=z*y) end); y:=1; call fac end") s) "y"

blockTest :: State -> Z
blockTest s = (s_dynmaic (myParse "begin var z:=x; proc p is (y:=z*z) end") s) "y"

myTestWhile :: State -> Z
myTestWhile s = (s_dynmaic (myParse "y:=1; while !(x=1) do (y:=y*x; x:=x-1)") s) "y"


-- Stm Natural Semantics (Proc Language Dynamic Variables and Static Procedures)
--------------------------------------------------------------------------------

data SEnv = EnvS (Stm) (Envs)

type Envs = Pname -> SEnv

s_mixed_ev :: Stm -> Envs -> State -> State
s_mixed_ev (Ass v a) e s        = update s (a_val a s) v
s_mixed_ev (Skip) e s           = s
s_mixed_ev (Comp stm1 stm2) e s = s_mixed_ev stm2 e (s_mixed_ev stm1 e s)
s_mixed_ev (If b stm1 stm2) e s = cond (b_val b, s_mixed_ev stm1 e, s_mixed_ev stm2 e) s
s_mixed_ev (While b stm) e s    = fix f s where
                            f g = cond( (b_val b),(g.(s_mixed_ev stm e)),s_mixed_ev Skip e)
s_mixed_ev (Block v p stm) e s  = reset_state s (s_mixed_ev stm (eval_decP_s p e) (eval_decV v s)) v
s_mixed_ev (Call name) e s      = s_mixed_ev (find_stm (e name) name ) (get_e (e name)) s --e == current proc environment we need the one q is called from



              -- Updates the procedure environment --

update_envs :: Envs -> Stm -> Pname -> Envs
update_envs e stm name =  envs where
                      envs x
                        | x == name = (EnvS stm envs) -- TODO this does not allow us to go back to previous def on proc
                        | otherwise = e x --all other procs should be called from block above definition



                        -- Evaluates DecP --

eval_decP_s :: DecP -> Envs -> Envs
eval_decP_s [] e = e
eval_decP_s (x:xs) e = eval_decP_s xs (update_envs e (snd x) (fst x))


              -- Returns the environment to execute --

get_e :: SEnv -> Envs
get_e  (EnvS s e) = e

              -- Returns the statement to execute --

find_stm :: SEnv -> Pname -> Stm
find_stm (EnvS stm envs) name = return_stm (envs name) where
  return_stm :: SEnv -> Stm
  return_stm (EnvS stm' envs') = stm'

-- Mixed Wrapper Functions --
--------------------------------------------------------------------------------

envs :: Envs
envs "hello" = (EnvS Skip envs)

s_mixed :: Stm -> State -> State
s_mixed stm s = s_mixed_ev stm envs s


-- Mixed Testing Functions --
--------------------------------------------------------------------------------

--Test State
state_mixed :: State
state_mixed "x" = 4
state_mixed "y" = 2

facmix :: State -> Z
facmix s = (s_mixed (myParse "begin proc fac is (begin var z:=x; if (x=1) then (skip) else (x:=x-1; call fac; y:=z*y) end); y:=1; call fac end") s) "y"

scopeTestmix :: State -> Z
scopeTestmix s = (s_mixed (myParse "begin var x:=0; proc p is (x:=x*2); proc q is (call p); begin var x:=5; proc p is (x:=x+1); call q; y:=x end end") s) "y"

mutual_mix :: State -> Z
mutual_mix s = (s_mixed (myParse "begin proc even is (begin if x = 0 then (y:=1) else (x:=x-1); call odd end); proc odd is (begin if x = 0 then (y:=0) else (x:=x-1); call even end); call even end") s) "y"


-- Stm Natural Semantics (Proc Language Static Variables and Static Procedures)
--------------------------------------------------------------------------------

                    -- Data types for static scope --
type VEnv  = Var -> Loc
type Loc   = Z
type Store = Loc -> Z

newtype SEnvP = SEnvP (Pname -> (Stm, VEnv, SEnvP, DecP))


--Updates the next value
new :: Loc -> Loc
new ext = ext + 1

--A constant representing the location of the next location a variable can be stord
next :: Loc
next = -1


                  -- Static evaluation of statements --

s_static_ev :: Stm -> SEnvP -> VEnv -> Store -> Store
s_static_ev (Ass v a) e venv sto = update_sto venv sto v (a_val a (sto.venv))
s_static_ev (Skip) e venv sto = sto
s_static_ev (Comp stm1 stm2) e venv sto = (s_static_ev stm2 e venv (s_static_ev stm1 e venv sto))
s_static_ev (If b stm1 stm2) e venv sto
  | b_val b (sto.venv)  = s_static_ev stm1 e venv sto
  | otherwise           = s_static_ev stm2 e venv sto
s_static_ev (While b stm) e venv sto
  | b_val b (sto.venv)  = s_static_ev (While b stm) e venv (s_static_ev stm e venv sto)
  | otherwise           = s_static_ev (Skip) e venv sto
s_static_ev (Block v p stm) e venv sto = s_static_ev stm e' venv' sto'
    where
      (venv',sto') = update_se v venv sto
      e'           = update_proc e venv p p
s_static_ev (Call name) (SEnvP e) venv sto = s_static_ev stm e'' venv' sto
    where
      (stm, venv',  e', dp) = e name
      e''                          = update_proc e' venv' dp dp

        -- Update the store by adding a new variable location to it --

update_sto :: VEnv -> Store -> Var -> Z -> Store
update_sto venv sto v a = nsto
  where nsto x
          | x == venv v = a
          | otherwise   = sto x


        -- Update the variable environment with the new vairiables --

update_se :: DecV -> VEnv -> Store -> (VEnv,Store)
update_se [] venv sto = (venv,sto)
update_se ((v,a):xs) venv sto = update_se xs nvenv nsto where
                      nvenv y                   --new venv
                        | y == v = (sto next)    --set the new variable to have the new available location
                        | otherwise = venv y      --set all other variables to their original location
                      nsto x                    --new store
                        | x == (sto next) = (a_val a (sto.venv))        --if the var does exist or is new update its value
                        | x == next = (new (sto next)) --set next to the next incremented by one
                        | otherwise = sto x       --set all pre-existing vars in the store to be the same


            -- Update the procedure environment --

update_proc :: SEnvP -> VEnv -> DecP -> DecP -> SEnvP
update_proc e venv [] dp           = e
update_proc (SEnvP e) venv ((p,stm):xs) dp = update_proc (SEnvP nenvs) venv xs dp where
                      nenvs x
                        | x == p = (stm, venv, (SEnvP e), dp)
                        | otherwise = e x


-- Wrapper Functions --
--------------------------------------------------------------------------------
--1)Get set of variables in stm and associate with a location

form_state :: VEnv -> Store -> State
form_state venv sto = (sto.venv)

s_static :: Stm -> State -> State
s_static stm s = (s_static_ev stm envst' envV sto) . envV
  where (envV, sto) = set_env (nub (get_vars stm)) venvs initial_store s

--s_static :: Stm -> State -> State
--s_static stm s = form_state venv ( s_static_ev stm e venv sto )
--  where
--    (venv,sto)  = (initial_venv stm s)
--    e    = SEnvP'
--    sto' next = 1
--    sto' x    = 0

get_vars :: Stm -> [Var]
get_vars (Ass v a)       = [v]
get_vars (Skip)          = []
get_vars (Comp s1 s2)    = (get_vars s1) ++ (get_vars s2)
get_vars (If b s1 s2)    = (get_vars s1) ++ (get_vars s2)
get_vars (While b stm)   = (get_vars stm)
get_vars (Block v p stm) = (get_vars stm) ++ (get_dv_vs v) ++ (get_dp_vs p)
get_vars (Call name)     = []

get_dv_vs :: DecV -> [Var]
get_dv_vs []          = []
get_dv_vs ((v,a):dvs) = v : get_dv_vs dvs

get_dp_vs :: DecP -> [Var]
get_dp_vs []            = []
get_dp_vs ((p,stm):dps) =  get_vars stm ++ get_dp_vs dps

initial_store :: Store
initial_store l
        | l == next = 1
        | otherwise  = 0


initial_venv :: Stm -> State -> (VEnv,Store)
initial_venv stm s = set_env  (nub (get_vars stm)) venvs initial_store s


set_env :: [Var] -> VEnv -> Store -> State-> (VEnv,Store)
set_env [] venv sto s     = (venv,sto)
set_env (v:vs) venv sto s = (set_env vs venv' sto' s)
  where
    venv' var
      | var == v  = (sto next)
      | otherwise = venv var
    sto' x
      | x == (sto next) = s v
      | x == next = (new (sto next))
      | otherwise = sto x


envst' :: SEnvP
envst' = SEnvP (\_ -> error "poop")

venvs :: VEnv
venvs x = 0

-- Static Testing Functions --
--------------------------------------------------------------------------------

--Static test state
state_static :: State
state_static "x" = 4
state_static  _  = 0

testVars ::  [Var]
testVars = get_vars(myParse "begin proc even is (begin if x = 0 then (y:=1) else (x:=x-1); call odd end); proc odd is (begin if x = 0 then (y:=0) else (x:=x-1); call even end); call even end")

scopeTeststatic :: State -> Z
scopeTeststatic testing = (s_static (myParse "begin var x:=0; proc p is (x:=x*2); proc q is (call p); begin var x:=5; proc p is (x:=x+1); call q; y:=x end end") testing) "y"

facstatic :: State -> Z
facstatic testing = (s_static (myParse "begin proc fac is (begin var z:=x; if (x=1) then (skip) else (x:=x-1; call fac; y:=z*y) end); y:=1; call fac end") testing) "y"

mutual_static :: Z
mutual_static = (s_static (myParse "begin proc even is begin if x = 0 then y:=1 else x:=x-1; call odd end; proc odd is begin if x = 0 then y:=0 else x:=x-1; call even end; call even end") state_static) "x"

mutualtest :: String
mutualtest = "/* blam */ begin proc even is begin if x = 0 then (y:= 1) else x := x -1; call odd end; proc odd is begin if x = 0 then y:= 0 else x := x-1; call even end; call even end"


parseTesting :: Stm
parseTesting = myParse "begin proc even is (begin if x = 0 then (y:=1) else (x:=x-1); call odd end); proc odd is (begin if x = 0 then (y:=0) else (x:=x-1); call even end); call even end"

--------------------------------------------------------------------------------
