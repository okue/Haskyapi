{-# LANGUAGE RecordWildCards, BangPatterns #-}
module Config.Parser (
  sparser,
  A (..),
) where
import Text.Parsec
import Control.Applicative ((<$>), (<*>))

data A = A { key :: String, aval :: String }
       | B { key :: String, bval :: [A] }
       deriving (Show)


sptab = many (oneOf " \t")
nt = oneOf "\n" *> (oneOf "\t" <|> char ' ' *> char ' ')

sparser :: String -> String -> Either ParseError [A]
sparser name str = parse (_sparser []) name str
  where
    _sparser l = do
      sharp
      many (oneOf "\n")
      a <- colon
      many (oneOf "\n")
      sharp
      try (eof *> return (a:l)) <|> _sparser (a:l)

colon = do
  x <- many alphaNum
  sptab *> char ':' *> sptab
  try (B x <$> (nt *> hyphens)) <|> (A x <$> many1 (alphaNum <|> oneOf "."))

arrow = do
  x <- many alphaNum
  sptab *> string "->" *> sptab
  A x <$> many1 (alphaNum <|> oneOf "./")

hyphens = _hyphens []
  where
    _hyphens l = do
      a <- hyphen
      try (sptab *> nt *> _hyphens (a:l)) <|> return (a:l)

hyphen = do
  char '-' *> sptab
  a <- arrow
  return a

sharp =
  try
    (oneOf "#" *> many (noneOf "\n") *> many (oneOf "\n") *> sharp)
  <|>
    return ()

main = do
  f <- readFile "./setting.yml"
  putStrLn f
  print $ sparser "setting.yml" f
  -- parseTest sparser "x:  y\nip : 0.0.0.0\n\nsub :\n\t- hoge -> foo\n\t- foo -> okok"
  -- parseTest sharp   "## comment out\n# comment\t is\n##########\n"
