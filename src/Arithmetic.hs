{-# LANGUAGE GADTs #-}

module Arithmetic where

import Control.Applicative hiding ((<|>), (<*>))
import Data.Bifunctor
import Data.Functor
import Data.Monoid ((<>))
import Debug.Trace
import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.String
import Text.Parsec.Token   hiding (parens)
-- import Text.Attoparsec

import Text.PrettyPrint ((<+>))
import qualified Text.PrettyPrint as Pretty


class Pretty p where
  ppr :: Int -> p -> Pretty.Doc

  {-# INLINE pp #-}
  pp :: p -> Pretty.Doc
  pp = ppr 0

  {-# INLINE ppg #-}
  ppg :: p -> String
  ppg = Pretty.render . pp


data Prim2 = PSum | PDiff | PProd | PDiv | PRange | PPow
  deriving (Eq, Show)

instance Pretty Prim2 where
  ppr _ PSum = Pretty.char '+'
  ppr _ PDiff = Pretty.char '-'
  ppr _ PProd = Pretty.char '*'
  ppr _ PDiv = Pretty.char '/'
  ppr _ PRange = Pretty.text "->"
  ppr _ PPow = Pretty.char '^'

data Prim1 = PNegate | PExpE | PExp10 | PLogE | PLog10 | PToDb
  deriving (Eq, Show)

instance Pretty Prim1 where
  ppr _ PNegate = Pretty.char '-'
  ppr _ PExpE = Pretty.text "exp"
  ppr _ PExp10 = Pretty.text "exp10"
  ppr _ PLogE = Pretty.text "log"
  ppr _ PLog10 = Pretty.text "log10"
  ppr _ PToDb = Pretty.text "dB"

evalPrim2 :: Prim2 -> UVal -> UVal -> UVal
evalPrim2 PSum (VLit x) (VLit y) = VLit $ (+) x y
evalPrim2 PDiff (VLit x) (VLit y) = VLit $ (-) x y
evalPrim2 PProd (VLit x) (VLit y) = VLit $ (*) x y
evalPrim2 PDiv  (VLit x) (VLit y) = VLit $ (/) x y
evalPrim2 PRange (VLit x) (VPair lo hi) = VLit $ (x-lo)/(hi-lo)
evalPrim2 PPow (VLit x) (VLit y) = VLit $ x ** y

evalPrim1 :: Prim1 -> UVal -> UVal
evalPrim1 PNegate (VLit x) = VLit $ negate x
evalPrim1 PExpE (VLit x) = VLit $ exp x
evalPrim1 PExp10 (VLit x) = VLit $ (10 **) x
evalPrim1 PLogE (VLit x) = VLit $ log x
evalPrim1 PLog10 (VLit x) = VLit $ logBase 10 x
evalPrim1 PToDb (VLit x) = VLit $ (20 *) . logBase 10 $ x


data UExp = ULit Double
          | UVar
          | UPrim2 Prim2 UExp UExp
          | UPrim1 Prim1 UExp
          | UPair  UExp  UExp
  deriving (Eq, Show)

instance Pretty UExp where
  ppr _ (ULit x) = Pretty.double x
  ppr _ UVar   = Pretty.char 'x'
  ppr _ (UPrim2 p a b) = Pretty.hsep [pp a, pp p, pp b]
  ppr _ (UPrim1 PToDb a) = pp a <+> pp PToDb
  ppr _ (UPrim1 p a) = pp p <+> pp a
  ppr _ (UPair a b) = Pretty.parens ((pp a <> Pretty.comma) <+> pp b)

ueval :: Double -> UExp -> UVal
ueval _ (ULit a) = VLit a
ueval _ (UPair (ULit a) (ULit b)) = VPair a b
ueval x UVar     = VLit x
ueval x (UPrim2 o a b) = evalPrim2 o (ueval x a) (ueval x b)
ueval x (UPrim1 o a) = evalPrim1 o (ueval x a)

uevalD :: Double -> UExp -> Double
uevalD x e = case ueval x e of
  VLit d -> d
  _      -> error "Arithmetic type error"

data UVal = VLit Double
          | VPair Double Double

parseUexp :: String -> Either String UExp
parseUexp s = bimap show id $ parse expr "string" s

ptest :: Show a => Parser a -> String -> String
ptest p s = show $ bimap show id $ parse p "test" s

expr = buildExpressionParser opTable term
       <?> "expression"

term = try litPair
       <|> try (between (char '(') (char ')') expr
       <|> lit
       <|> (char 'x' >> pure UVar)) <* spaces
       <?> "simple expression"

tokP = makeTokenParser emptyDef
opTable = [ [prefix "-" (UPrim1 PNegate),
             prefix "log10" (UPrim1 PLog10),
             prefix "log" (UPrim1 PLogE),
             prefix "exp10" (UPrim1 PExp10),
             prefix "exp" (UPrim1 PExpE),
             postfix "dB" (UPrim1 PToDb)]
          , [binary "^" (UPrim2 PPow) AssocLeft]
          , [binary "*" (UPrim2 PProd) AssocLeft,
             binary "/" (UPrim2 PDiv) AssocLeft]
          , [binary "+" (UPrim2 PSum) AssocLeft,
             binary "-" (UPrim2 PDiff) AssocLeft,
             binary "->" (UPrim2 PRange) AssocLeft]
          ]

binary name fun assoc = Infix (do{ reservedOp tokP name; return fun}) assoc
prefix name fun = Prefix (do{reservedOp tokP name; return fun})
postfix name fun = Postfix (do{reservedOp tokP name; return fun})

litPair :: Parser UExp
litPair = between (char '(') (char ')') $ do
  l1 <- lit
  spaces
  char ','
  spaces
  l2 <- lit
  return $ UPair l1 l2


lit :: Parser UExp
lit = do
  n <- optionMaybe (char '-')
  v <- either fromIntegral id <$>
       naturalOrFloat (makeTokenParser emptyDef)
  case n of
    Nothing -> return $ ULit v
    Just _  -> return (ULit $ negate v)

-- op2 :: Parser Prim2
-- op2 = try (char '+' $> PSum)
--   <|> try (char '-' $> PDiff)
--   <|> try (char '*' $> PProd)
--   <|> try (char '/' $> PDiv)

-- binarySequence :: Parser [UExp]
-- binarySequence =
--   many1 (try parens <|> try prim1 <|> try var <|> lit)

-- op1 :: Parser Prim1
-- op1 = try (char '-'       $> PNegate)
--   <|> try (string "exp10" $> PExp10)
--   <|> try (string "exp"   $> PExpE)
--   <|> try (string "log10" $> PLog10)
--   <|> try (string "log"   $> PLogE)

-- prim2 :: Parser UExp
-- prim2 = do
--   a <- expr
--   spaces
--   o <- op2
--   spaces
--   b <- expr
--   return (UPrim2 o a b)

-- -- negate :: Parser UExp
-- -- negate = do
-- --   char 

-- prim1 :: Parser UExp
-- prim1 = try noSpaceParens <|> spaceLitOrVar
--   where noSpaceParens = do
--           o <- op1
--           e <- parens
--           return (UPrim1 o e)
--         spaceLitOrVar = do
--           o <- op1
--           many1 (char ' ')
--           l <- var <|> lit
--           return (UPrim1 o l)

-- var :: Parser UExp
-- var = char 'x' >> pure UVar






-- data ArithExpr a where
--   ALit   :: Double -> ArithExpr Double
--   AIVar  :: ArithExpr Double
--   APrim2 :: Prim2
--          -> ArithExpr Double
--          -> ArithExpr Double
--          -> ArithExpr Double
--   APrim1 :: Prim1 -> ArithExpr Double -> ArithExpr Double
--   AParens :: ArithExpr a -> ArithExpr a

-- eval :: Double -> ArithExpr a -> Double
-- eval _ (ALit r)        = r
-- eval x AIVar           = x
-- eval x (APrim2 p2 a b) = evalPrim2 p2 (eval x a) (eval x b)
-- eval x (APrim1 p1 a)   = evalPrim1 p1 (eval x a)
-- eval x (AParens a)     = eval x a

