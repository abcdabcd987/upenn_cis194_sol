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
extend state var val = \input -> if input == var then val else state input

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE state (Var var) = state var
evalE state (Val val) = val
evalE state (Op lhs op rhs)
  | op == Plus   = a + b
  | op == Minus  = a - b
  | op == Times  = a * b
  | op == Divide = a `div` b
  | op == Gt     = toInt $ a >  b
  | op == Ge     = toInt $ a >= b
  | op == Lt     = toInt $ a <  b
  | op == Le     = toInt $ a <= b
  | op == Eql    = toInt $ a == b
  where a = evalE state lhs
        b = evalE state rhs
        toInt x = if x then 1 else 0

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign target expr) = DAssign target expr
desugar (If cond ifTrue ifFalse) = DIf cond (desugar ifTrue) (desugar ifFalse)
desugar (While cond body) = DWhile cond $ desugar body
desugar (Sequence a b) = DSequence (desugar a) (desugar b)
desugar Skip = DSkip
desugar (Incr var) = DAssign var (Op (Var var) Plus (Val 1))
desugar (For init cond step body) =
  DSequence (desugar init) (DWhile
    cond
    (DSequence (desugar body) (desugar step))
  )


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple state (DAssign var expr) = extend state var $ evalE state expr
evalSimple state (DIf cond ifTrue ifFalse) =
  if evalE state cond /= 0
  then evalSimple state ifTrue
  else evalSimple state ifFalse
evalSimple state while@(DWhile cond body) =
  if evalE state cond /= 0
  then evalSimple state' while
  else state
  where state' = evalSimple state body
evalSimple state (DSequence a b) = evalSimple (evalSimple state a) b
evalSimple state DSkip = state

run :: State -> Statement -> State
run state stmt = evalSimple state $ desugar stmt

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
