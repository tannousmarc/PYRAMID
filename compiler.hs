import Prelude hiding (Num)
import qualified Prelude (Num)

type Num = Integer
type Var = String

type Z = Integer
type T = Bool

type State = Var -> Z

data Aexp = N Num | V Var |
            Mult Aexp Aexp | Add Aexp Aexp | Sub Aexp Aexp
            deriving (Show, Eq, Read)

data Bexp = TRUE | FALSE |
            Neg Bexp | And Bexp Bexp | Eq Aexp Aexp | Le Aexp Aexp
            deriving (Show, Eq, Read)

n_val :: Num -> Z
n_val = id

s :: State
s "x" = 1
s "y" = 2
s "z" = 3
s  _  = 0

a :: Aexp
a = Mult (Add (V "x") (V "y")) (Sub (V "z") (N 1))

a_val :: Aexp -> State -> Z
a_val (N n) s = n
a_val (V v) s = s v
a_val (Mult a b) s = (a_val a s) * (a_val b s)
a_val (Add a b) s = (a_val a s) + (a_val b s)
a_val (Sub a b) s = (a_val a s) - (a_val b s)

b :: Bexp
b = Neg (Eq (Add (V "x") (V "y")) (N 4))

b_val :: Bexp -> State -> T
b_val (TRUE) s = True
b_val (FALSE) s = False
b_val (Neg b) s = not (b_val b s)
b_val (And a b) s = (b_val a s) && (b_val b s)
b_val (Eq a b) s = (a_val a s) == (a_val b s)
b_val (Le a b) s = (a_val a s) <= (a_val b s)

data Stm = Ass Var Aexp | Skip |
           Comp Stm Stm | If Bexp Stm Stm | While Bexp Stm
           deriving (Show, Eq, Read)

p :: Stm
p = (Comp
  (Ass "y" (N 1))
  (While
  (Neg (Eq (V "x") (N 1)))
    (Comp
    (Ass "y" (Mult (V "y") (V "x")))
    (Ass "x" (Sub (V "x") (N 1)))
    )
  )
  )

update :: State -> Z -> Var -> State
update s i v = s'
  where
    s' x | x == v = i
         | otherwise = s x

s' :: State
s' = update s 5 "x"

cond :: (a->T, a->a, a->a) -> (a->a)
cond (b, c, d) x
  | b x == True = c x
  | b x == False = d x

fix :: ((State -> State) -> (State -> State)) -> (State -> State)
fix ff = ff (fix ff)

s_ds :: Stm -> State -> State
s_ds (Ass v a) s = update s (a_val a s) v
s_ds (Skip) s = id s
s_ds (Comp s1 s2) s = ((s_ds s2) . (s_ds s1)) s
s_ds (If b s1 s2) s = cond((b_val b), (s_ds s1), (s_ds s2)) s
s_ds (While b st) s = fix (\g -> cond ((b_val b), g . (s_ds st) , id)) s