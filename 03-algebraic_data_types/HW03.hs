module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop =
    Plus
  | Minus
  | Times
  | Divide
  | Gt
  | Ge
  | Lt
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend input variable value = output
    where output :: State
          output x = if x==variable then value else input x

empty :: State
empty x = 0

-- Exercise 2 -----------------------------------------

-- I add uncurry so that the function composition can be done
evalE :: State -> Expression -> Int
evalE st (Op ex1 b ex2) = interpretBop b (evalE st ex1, evalE st ex2)
    where boolToInt :: Bool -> Int
          boolToInt True = 1
          boolToInt False = 0
          interpretBop :: Bop -> ((Int, Int) -> Int)
          interpretBop b = case b of
                             Plus -> uncurry (+)
                             Minus -> uncurry (-)
                             Times -> uncurry (*)
                             Divide -> uncurry quot
                             Gt -> boolToInt . uncurry (>)
                             Ge -> boolToInt . uncurry (>=)
                             Lt -> boolToInt . uncurry (<)
                             Le -> boolToInt . uncurry (<=)
                             Eql -> boolToInt . uncurry (==)
evalE _ (Val val) = val
evalE st (Var var) = st var

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (For init condition update body) = DSequence (desugar init) (DWhile condition (DSequence (desugar body) (desugar update)))
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (While x st) = DWhile x (desugar st)
desugar (Assign a b) = DAssign a b
desugar (Sequence st1 st2) = DSequence (desugar st1) (desugar st2)
desugar (If x st1 st2) = DIf x (desugar st1) (desugar st2)
desugar Skip = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign var expr) = extend st var (evalE st expr)
evalSimple st (DIf expr diet1 diet2)
    | evalE st expr == 1 = evalSimple st diet1
    | otherwise = evalSimple st diet2
evalSimple st (DSequence diet1 diet2) = evalSimple (evalSimple st diet1) diet2
evalSimple st (DWhile expr diet)
    | evalE st expr == 1 = evalSimple (evalSimple st diet) (DWhile expr diet)
    | otherwise = st
evalSimple st DSkip = st


run :: State -> Statement -> State
run state statement = evalSimple state (desugar statement)

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
