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

evalS :: BBAE -> (Maybe BBAE)
evalS (Num n) = Just (Num n)-- (subst n n (Num n))
evalS _ = Nothing

evalM :: Env -> BBAE -> (Maybe BBAE)
evalM e (Num n) = Just (Num n)
-- evalM e (Plus l r) = do (Just (Plus (evalM e (Num l)) (evalM e (Num r))))
evalM _ _ = Nothing

testBBAE :: BBAE -> Bool
testBBAE _ = True

typeofM :: Cont -> BBAE -> (Maybe TBBAE)
typeofM c (Num n) = Just TNum
typeofM c (Boolean b) = Just TBool
-- typeofM c (Plus l r) = do {
--  TNum <- typeofM l;
--  TNum <- typeofM r;
--  Just TNum
-- }
typeofM _ _ = Nothing

evalT :: BBAE -> (Maybe BBAE)
evalT _ = Nothing
