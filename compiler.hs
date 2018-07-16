import Prelude hiding (Num)
import qualified Prelude (Num)

type Num = Integer
type Var = String

type Z = Integer
type T = Bool

type State = Var -> Z

data Aexp = Num Num | Var Var |
            Aexp :*: Aexp | Aexp :+: Aexp | Aexp :-: Aexp
            deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE |
            NOT Bexp | Bexp :&&: Bexp | Aexp :=: Aexp | Aexp :<=: Aexp
            deriving (Show, Eq, Read)

n_val :: Num -> Z
n_val = id

s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s  _  = 0

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

data Stm = Var ::=: Aexp | Skip |
           Stm :.: Stm | If Bexp Stm Stm | While Bexp Stm
           deriving (Show, Eq, Read)


update :: State -> Z -> Var -> State
update s i v = s'
  where
    s' x | x == v = i
         | otherwise = s x

p :: Stm
p = ("y" ::=: Num 1) :.:
    ("x" ::=: Num 10) :.:
    While (NOT (Var "x" :=: Num 1))
    (("y" ::=: (Var "y" :*: Var "x")) :.:
    ("x" ::=: (Var "x" :-: Num 1)))

cond :: (a->T, a->a, a->a) -> (a->a)
cond (b, c, d) x
  | b x == True = c x
  | b x == False = d x

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (v ::=: a) s = update s (a_val a s) v
s_ds (Skip) s = id s
s_ds (s1 :.: s2) s = ((s_ds s2) . (s_ds s1)) s
s_ds (If b s1 s2) s = cond((b_val b), (s_ds s1), (s_ds s2)) s
s_ds (While b st) s = fix (\g -> cond ((b_val b), g . (s_ds st) , id)) s

runScript :: Stm -> State -> ([Var] -> [Z])
runScript p s = map (s_ds p s)
