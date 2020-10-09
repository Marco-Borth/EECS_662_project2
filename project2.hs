-- file Name: project2.hs
-- file Author: Marco Borth, 2894114
-- description: project2 file containing adding booleans and type checking fucntions.
{-# LANGUAGE GADTs,FlexibleContexts #-}

-- Imports for Monads

import Control.Monad

-- BBAE AST and Type Definitions

data TBBAE where
  TNum :: TBBAE
  TBool :: TBBAE
  deriving (Show,Eq)

data BBAE where
  Num :: Int -> BBAE
  Plus :: BBAE -> BBAE -> BBAE
  Minus :: BBAE -> BBAE -> BBAE
  Bind :: String -> BBAE -> BBAE -> BBAE
  Id :: String -> BBAE
  Boolean :: Bool -> BBAE
  And :: BBAE -> BBAE -> BBAE
  Leq :: BBAE -> BBAE -> BBAE
  IsZero :: BBAE -> BBAE
  If :: BBAE -> BBAE -> BBAE -> BBAE
  deriving (Show,Eq)

type Env = [(String,BBAE)]

type Cont = [(String,TBBAE)]

evalAE :: BBAE -> Int
evalAE (Num n) =
  if n >= 0
    then n :: Int
    else error "ERROR: Only Natural Numbers are Allowed"

evalAE (Boolean b) = error "ERROR: Boolean Detected"

evalAE (Plus l r) =
  let x = evalAE(l)
      y = evalAE(r)
      in x + y

evalAE (Minus l r) =
  let x = evalAE(l)
      y = evalAE(r)
      in if x >= y
        then x - y
        else error "ERROR: Resulting Difference must be Natural"

subst::String -> BBAE -> BBAE -> BBAE
subst x v (Num n) = Num (evalAE (Num n))
subst x v (Boolean b) = Boolean b
subst x v (Plus l r) = Num (evalAE (Plus l r)) -- Plus (subst x v l) (subst x v r)
subst x v (Minus l r) = Num(evalAE (Minus l r))
subst x v (Bind a b c) = Bind a (subst x v b) (subst x v c)
subst x v (Id a) =
  if x == a
    then v
    else Id a

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) =
  let x = [("n", Num n)]
    in evalM x (Num n)

evalS (Boolean b) =
  let x = [("b", Boolean b)]
    in evalM x (Boolean b)

evalS (Plus l r) = Just (Num (evalAE (Plus l r) ) )
--   let x = [("l", l), ("r", r)]
--    in Just ( subst  x l (Plus l r) ) -- evalM x (Plus x r)
  -- Just (Num (evalAE (Plus l r) ) ) -- (subst  [("l", l)] [("r", r)] (Plus l r)) --(Num (evalAE (Plus l r) ))
evalS (Minus l r) = Just (Num (evalAE (Minus l r) ) ) -- ( subst l r (Minus l r))
evalS (Bind s a b) = do {
 let x = evalAE (a)
  in evalS (subst s (Num x) b)
}
--lookup Id x
evalS (Id x) = Nothing
--do {
--  let a = [("x", x)]
--    in Just ( subst x x (Id x) ) -- ( Num (read "x" :: Int) ) ( Num (read "x" :: Int) ) )
--}
evalS (And l r) = Just ( Boolean ( testBBAE ( And l r) ) )
evalS (Leq l r) = Just ( Boolean ( testBBAE ( Leq l r) ) )
evalS (IsZero x) = Just ( Boolean ( testBBAE (IsZero x) ) )
evalS (If c t e) = Just ( Boolean ( testBBAE ( If c t e ) ) )

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM e (Num n) = Just (Num n)
evalM e (Boolean b) = Just (Boolean b)
evalM e (Plus l r) = Just (Num (evalAE (Plus l r) ) )
evalM e (Minus l r) = Just (Num (evalAE (Minus l r) ) ) -- ( subst l r (Minus l r))
--let x = evalM e (a)
  --in evalM e ( (s,x) b)
evalM e (Bind s a b) = do {
 let x = evalAE (a)
  in evalM e (subst s (Num x) b)
}
evalM e (Id x) = Nothing
-- do {
--  let a = ["x", x]
--    in Just ( subst x ( Num (read "x" :: Int) ) ( Num (read "x" :: Int) ) )
-- }
evalM e (And l r) = Just ( Boolean ( testBBAE ( And l r) ) )
evalM e (Leq l r) = Just ( Boolean ( testBBAE ( Leq l r) ) )
evalM e (IsZero x) = Just ( Boolean ( testBBAE (IsZero x) ) )
evalM e (If c t x) = Just ( Boolean ( testBBAE ( If c t x ) ) )



testBBAE :: BBAE -> Bool
testBBAE (Num n) =
  let x = [("n", Num n)]
    in if evalS (Num n) == evalM x (Num n)
      then True
      else False

testBBAE (Boolean b) =
  if Boolean b == Boolean True
    then True
    else False

testBBAE (Plus l r) =
  let x = [("l", l), ("r", r)]
    in if evalS (Plus l r) == evalM x (Plus l r)
      then True
      else False

testBBAE (Minus l r) =
  let x = [("l", l), ("r", r)]
    in if evalS (Minus l r) == evalM x (Minus l r)
      then True
      else False

testBBAE (Bind s a b) =
  let x = [("a", a), ("b", b)]
    in if evalS (Bind s a b) == evalM x (Bind s a b)
      then True
      else False

testBBAE (Id x) =
  let y = [("x", x)]
    in if evalS (Id x) == evalM [] (Id x)
      then True
      else False

testBBAE (And l r) =
  if l == Boolean True
    then if r == Boolean True
      then True
      else False
    else False

testBBAE (Leq l r) =
  if evalAE l <= evalAE r
    then True
    else False

testBBAE (IsZero x) | x == Num 0 = True
                    | otherwise = False

testBBAE (If c t e) =
    if c == Boolean True
      then testBBAE t
      else testBBAE e



typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM s (Num n) | n >= 0 = Just TNum
                  | n < 0 = error "ERROR: Only Natural Numbers are Allowed"
                  | otherwise = error "ERROR: Possible Boolean Detected"

typeofM s (Boolean b) | b == True = Just TBool
                      | b == False = Just TBool
                      | otherwise = error "ERROR: Possible Integer Detected"

typeofM s (Plus l r) =
  if typeofM s l == Just TNum
    then if typeofM s r == Just TNum
      then Just TNum
      else Just TBool
    else Just TBool

typeofM s (Minus l r) =
  if typeofM s l == Just TNum
    then if typeofM s r == Just TNum
      then Just TNum
      else Just TBool
    else Just TBool

typeofM s (Bind x a b) =
  if typeofM s a == Just TNum
    then Just TNum
    else Just TBool

typeofM s (Id x) = Nothing
--  if typeofM (evalM x (Num x)) == Just TNum
--    then Just TNum
--    else if typeofM (evalM x (Boolean x) )
--      then Just TBool
--      else error "ERROR: TYPE NOT FOUND"
-- subst x ( Num (read "x" :: Int) ) ( Num (read "x" :: Int) )
--  let x = [("x", x)]
--      in if typeofM s (subst x s ) == Just TNum
--        then Just TNum
--        else Just TBool

typeofM s (And l r) =
  if typeofM s l == Just TBool
    then if typeofM s r == Just TBool
      then Just TBool
      else Just TNum
    else Just TNum

typeofM s (Leq l r) =
  if typeofM s l == Just TNum
    then if typeofM s r == Just TNum
      then Just TBool
      else Just TNum
    else Just TNum

typeofM s (IsZero x) =
  if typeofM s x == Just TNum
    then Just TNum
    else Just TBool

typeofM s (If c t e) =
  if typeofM s c == Just TBool
    then Just TBool
    else Just TNum



evalT :: BBAE -> (Maybe BBAE)
evalT (Num n) = let s = [("n", TNum)]
  in if typeofM s (Num n) == Just TNum
    then Just (Num n)
    else Nothing

evalT (Boolean b) = let s = [("b", TBool)]
  in if typeofM s (Boolean b) == Just TBool
    then Just (Boolean b)
    else Nothing

evalT (Plus l r) =
  let s = [("l", TNum), ("r", TNum)]
      in if typeofM s (Plus l r) == Just TNum
        then evalS (Plus l r)
        else Nothing

evalT (Minus l r) =
  let s = [("l", TNum), ("r", TNum)]
      in if typeofM s (Minus l r) == Just TNum
        then evalS (Minus l r)
        else Nothing

evalT (Bind x a b) =
  let s = [("a", TNum), ("a", TBool), ("b", TNum), ("b", TBool)]
    in if typeofM s (Bind x a b) == Just TNum
      then evalS (Bind x a b)
      else evalS (Bind x a b)

evalT (Id x) = let s = [("n", TNum)]
  in if typeofM s (Id x) == Nothing
    then evalS (Id x)
    else Nothing

evalT (And l r) =
  let s = [("l", TBool), ("r", TBool)]
      in if typeofM s (And l r) == Just TBool
        then evalS (And l r)
        else Nothing

evalT (Leq l r) =
  let s = [("l", TNum), ("r", TNum)]
    in if typeofM s (Leq l r) == Just TBool
      then evalS (Leq l r)
      else Nothing

evalT (IsZero x) =
  let s = [("x", TNum)]
    in if typeofM s (IsZero x) == Just TNum
      then evalS (IsZero x)
      else Nothing

evalT (If c t e) =
  let s = [("c", TBool), ("t", TNum), ("t", TBool), ("c", TNum), ("c", TBool) ]
    in if typeofM s (If c t e) == Just TBool
      then evalS (If c t e)
      else Nothing
