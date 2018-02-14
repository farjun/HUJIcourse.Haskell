import Text.ParserCombinators.Parsec
import Test.QuickCheck.Function


--Q1
functorIdProp :: (Functor f, Eq (f a)) => f a -> Bool
functorIdProp x = (fmap id x) == x

functorCompProp :: (Functor f, Eq (f c)) => f a -> Fun a b -> Fun b c -> Bool
functorCompProp x (Apply -> f) (Apply -> g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

-- --Q2
-- data Expression = Var String | Lam Expression Expression | App Expression Expression
--   deriving Eq

-- instance Show Expression where
--   show (Var v) = v
--   show (Lam v e) = "/(" ++ show v ++ "." ++ show e ++ ")"
--   show (Application e1 e2) = "" ++ show e1 ++ " " ++ show e2 ++ ""

-- parseSource :: String -> Expression
-- parseSource source =
--   case parse (parseExpression <* eof) "" source of
--     Right e -> e
--     _ -> error "Parsing error"

-- parseExpression :: Parser Expression
-- parseExpression =
--   parseVar <|> parseExp <|> parseApp

-- parseVar :: Parser Expression
-- parseVar = Var <$> many1 letter

-- parseLam :: Parser Expression
-- parseLam = do
--   _ <- char '/'
--   _ <- char '('
--   arg <- parseVar
--   _ <- char '.'
--   body <- parseExpression
--   _ <- char ')'
--   return $ Lam arg body

-- parseApp :: Parser Expression
-- parseApp = do
--   e1 <- parseExpression
--   _ <- spaces
--   e2 <- parseExpression
--   return $ App e1 e2

{-# LANGUAGE ViewPatterns #-}
isOne::[Int]->Bool
isOne (head->1) = True
isOne _ = False