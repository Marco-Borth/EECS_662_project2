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

-- subst::string -> BBAE -> BBAE -> BBAE
-- subst x v (Num n) = Bind x v n

subst :: BBAE -> Int
subst (Num n) =
  if n >= 0
    then n :: Int
    else error "ERROR: Only Natural Numbers are Allowed"

subst (Boolean b) = error "ERROR: Boolean Detected"

subst (Plus l r) =
  let x = subst(l)
      y = subst(r)
      in x + y

subst (Minus l r) =
  let x = subst(l)
      y = subst(r)
      in if x >= y
        then x - y
        else error "ERROR: Resulting Difference must be Natural"

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = Just (Num n) -- (subst n n (Num n))
evalS (Boolean b) = Just (Boolean b)
evalS (Plus l r) = Just (Num (subst (Plus l r) ))
evalS (Minus l r) = Just (Num (subst (Minus l r) ))
evalS (And l r) = Just ( Boolean ( testBBAE ( And l r) ) )
evalS (Leq l r) = Just ( Boolean ( testBBAE ( Leq l r) ) )
evalS (IsZero x) = Just ( Boolean ( testBBAE (IsZero x) ) )
evalS (If c t e) = Just ( Boolean ( testBBAE ( If c t e ) ) )

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM e (Num n) = Just (Num n)

evalM e (Boolean b) = Just (Boolean b)

-- evalM e (Plus l r) =
--  let s = [("l",(Num l)), ("r", (Num r))]
--  in
-- evalM e (Plus l r) = do (Just (Plus (evalM e (Num l)) (evalM e (Num r))))


testBBAE :: BBAE -> Bool
testBBAE (Boolean b) =
  if Boolean b == Boolean True
    then True
    else False

testBBAE (And l r) =
  if l == Boolean True
    then if r == Boolean True
      then True
      else False
    else False

testBBAE (Leq l r) =
  if subst l <= subst r
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

-- typeofM s (Id s) = typeofM c (Num s)

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
