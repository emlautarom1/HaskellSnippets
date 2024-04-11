{-# LANGUAGE LambdaCase #-}

module LaTeXToUTF8 (parseLaTeX, laTeXToUTF8) where

import Control.Applicative (Alternative ((<|>)))
import Data.Char qualified as Char
import Data.List (singleton)
import Text.ParserCombinators.ReadP

data LaTeX
  = Lit Char
  | Num Integer
  | Whitespace Int
  | Parens [LaTeX]
  | Func String [LaTeX]
  deriving (Show, Eq)

alphaP :: ReadP Char
alphaP = satisfy Char.isAlpha

symP :: ReadP Char
symP = satisfy (`elem` "+-*/%<=>.,;'")

whitespaceP :: ReadP LaTeX
whitespaceP = do
  spaces <- many1 (satisfy Char.isSpace)
  return $ Whitespace (length spaces)

parensP :: ReadP LaTeX
parensP = Parens <$> between (char '(') (char ')') latexP

litP :: ReadP LaTeX
litP = do
  Lit <$> (alphaP <|> symP)

numP :: ReadP LaTeX
numP = do
  Num . read <$> many1 (satisfy Char.isDigit)

operatorP :: ReadP LaTeX
operatorP = do
  op <- satisfy (`elem` "^_")
  args <- many (between (char '{') (char '}') latexP) <|> (singleton <$> latexP)
  return $ Func [op] (concat args)

funcP :: ReadP LaTeX
funcP = do
  op <- char '\\' *> many1 alphaP
  args <- many $ between (char '{') (char '}') latexP
  return $ Func op (concat args)

latexP :: ReadP [LaTeX]
latexP = many $ choice [whitespaceP, parensP, funcP, operatorP, numP, litP]

examples :: [(String, String)]
examples =
  [ ("bigO", "\\O")
  , ("bigO1", "\\O(1)")
  , ("fA", "\\f(\\a)")
  , ("frac", "\\frac{\\x}{\\y}")
  , ("log", "\\log")
  , ("pi", "\\pi")
  , ("logPi", "\\log(\\pi)")
  , ("union", "\\O\\bigl(m \\log\\bigl(\\frac{n}{m}+1\\bigr)\\bigr)")
  , ("superscript", "1^2")
  , ("mulSubscript", "\\O_{2\\pi}")
  , ("forall", "\\forall \\x \\in X, \\quad \\exists \\y \\leq \\epsilon")
  ]

parseLaTeX :: String -> [LaTeX]
parseLaTeX = fst . last . readP_to_S latexP

-- >>> parseLaTeX (snd $ examples !! 10)
-- [Func "forall" [],Whitespace 1,Func "x" [],Whitespace 1,Func "in" [],Whitespace 1,Lit 'X',Lit ',',Whitespace 1,Func "quad" [],Whitespace 1,Func "exists" [],Whitespace 1,Func "y" [],Whitespace 1,Func "leq" [],Whitespace 1,Func "epsilon" []]

toUTF8 :: LaTeX -> String
toUTF8 = \case
  (Lit c) -> [c]
  (Num n) -> show n
  (Whitespace _) -> " "
  (Parens es) -> "(" ++ concatMap toUTF8 es ++ ")"
  (Func "f" args) -> "𝑓" ++ concatMap toUTF8 args
  (Func "a" []) -> "𝑎"
  (Func "x" []) -> "𝑥"
  (Func "y" []) -> "𝑦"
  (Func "O" []) -> "𝑂"
  (Func "forall" []) -> "∀"
  (Func "in" []) -> "∈"
  (Func "exists" []) -> "∃"
  (Func "epsilon" []) -> "𝜖"
  (Func "pi" []) -> "π"
  (Func "leq" []) -> "≤"
  (Func "quad" []) -> " "
  (Func "bigl" []) -> []
  (Func "bigr" []) -> []
  (Func "log" args) -> "log" ++ concatMap toUTF8 args
  (Func "frac" [a, b]) -> toUTF8 a ++ "/" ++ toUTF8 b
  (Func name args) -> "\\" ++ name ++ concatMap (\arg -> "{" ++ toUTF8 arg ++ "}") args

laTeXToUTF8 :: String -> String
laTeXToUTF8 = concatMap toUTF8 . fst . last . readP_to_S latexP
