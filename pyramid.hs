import Prelude hiding (Num)
import qualified Prelude (Num)
import Parser
import Data.List
import Data.Maybe
import Yoda

type Num = Integer

type Z = Integer
type T = Bool

type State = Var -> Z

n_val :: Num -> Z
n_val = id

a_val :: Aexp -> State -> Z
a_val (Num n) s = n
a_val (Var v) s = s v
a_val (a :*: b) s = (a_val a s) * (a_val b s)
a_val (a :+: b) s = (a_val a s) + (a_val b s)
a_val (a :-: b) s = (a_val a s) - (a_val b s)

b_val :: Bexp -> State -> T
b_val (TRUE) s = True
b_val (FALSE) s = False
b_val (NOT b) s = not (b_val b s)
b_val (a :&&: b) s = (b_val a s) && (b_val b s)
b_val (a :=: b) s = (a_val a s) == (a_val b s)
b_val (a :<=: b) s = (a_val a s) <= (a_val b s)

update :: State -> Z -> Var -> State
update s i v = s'
  where
    s' x | x == v = i
         | otherwise = s x

cond :: (a->T, a->a, a->a) -> (a->a)
cond (b, c, d) x
  | b x == True = c x
  | b x == False = d x

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)

s_ds :: Stmnt -> State -> State
s_ds (v ::=: a) s = update s (a_val a s) v
s_ds (Skip) s = id s
s_ds (s1 :.: s2) s = ((s_ds s2) . (s_ds s1)) s
s_ds (If b s1 s2) s = cond((b_val b), (s_ds s1), (s_ds s2)) s
s_ds (While b st) s = fix (\g -> cond ((b_val b), g . (s_ds st) , id)) s

-- for demo purposes
basicState :: State
basicState  _  = 0

basicVars :: [Var]
basicVars = map (\x->x:[]) ['a'..'z']


-- factorial: "x:=10; y:=1; while !x=1 do y:=y*x; x:=x-1"
runScript :: String -> [Z]
runScript p = map (s_ds (snd (parse stmnt p !! 0)) basicState) basicVars

-- valueOf 'a' (runScript (a::=:Num 1))
valueOf :: Char -> [Z] -> Z
valueOf value list = list !! (fromMaybe 0 (elemIndex value ['a'..'z']))
