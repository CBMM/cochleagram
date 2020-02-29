{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Arithmetic where

import Control.Applicative hiding ((<|>), (<*>))
import Data.Bifunctor
import Data.Functor
import Data.Monoid ((<>))
import qualified Data.Text as Text
import Debug.Trace
import Text.Parsec
import Data.Bool (bool)
import Data.Maybe (isJust, fromMaybe)
import Text.Parsec.Language
import Text.Parsec.Expr
import Text.Parsec.String
import Reflex (Dynamic, holdDyn, fmapMaybe, updated)
import Text.Parsec.Token   hiding (parens)
import Reflex.Dom ((=:))
import Reflex.Dom.Old (MonadWidget)
import Reflex.Dom.Widget (def, el, text, value)
import Reflex.Dom.Widget.Input (textInput, TextInputConfig(..))

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


funExprInput :: MonadWidget t m => UExp -> m (Dynamic t UExp)
funExprInput exp0 = mdo
  let pExp0 = Pretty.render $ pp exp0
  -- let inputConfig = def
  let boxAttrs =
         (\ats e ->  ats <> "class" =: bool "expr expr-bad" "expr expr-good" (isJust e))
         <$> _textInputConfig_attributes def <*> textExpr
  eInp <- textInput def { _textInputConfig_initialValue = Text.pack pExp0
                        , _textInputConfig_attributes   = boxAttrs
                        }
  let textExpr = hush . parseUexp . Text.unpack <$> value eInp
  outExp   <- holdDyn exp0 $ fmapMaybe id (updated textExpr)
  el "br" (return ())
  return outExp


defaultRedExpr = UPrim2 PPow (UPrim2 PRange (UPrim1 PToDb UVar)
                                        (UPair (ULit (-90)) (ULit (-50))))
                           (ULit 4)
defaultGreenExpr = UPrim2 PPow (UPrim2 PRange (UPrim1 PToDb UVar)
                                        (UPair (ULit (-100)) (ULit (-70))))
                           (ULit 4)
defaultBlueExpr = UPrim2 PPow (UPrim2 PRange (UPrim1 PToDb UVar)
                                        (UPair (ULit (-90)) (ULit (-50))))
                           (ULit 2)


colorMappings :: MonadWidget t m => m (Dynamic t (UExp, UExp, UExp))
colorMappings = do
  rFunc <- text "R" >> funExprInput defaultRedExpr
  gFunc <- text "G" >> funExprInput defaultGreenExpr
  bFunc <- text "B" >> funExprInput defaultBlueExpr
  return $ (,,) <$> rFunc <*> gFunc <*> bFunc

    -- text "R"
    -- rFunc <- funExprInput
    --          def
    -- el "br" $ return ()
    -- text "G"
    -- gFunc <- funExprInput ()
    --          def
    -- el "br" $ return ()

    -- text "B"
    -- bFunc <- funExprInput ()
    --          def
    -- el "br" $ return ()

    -- let cFuncs = (,,) <$> rFunc <*> gFunc <*> bFunc
    -- -- cFuncs <- $(qDyn [| ( $(unqDyn [|rFunc|]), $(unqDyn [|gFunc|]), $(unqDyn [|bFunc|])) |])

hush :: Either l r -> Maybe r
hush (Left _)  = Nothing
hush (Right r) = Just r
