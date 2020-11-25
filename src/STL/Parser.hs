{-# LANGUAGE OverloadedStrings #-}

module STL.Parser where
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile)
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token    as Token
import Control.Monad (void)
import Text.Parsec.Number hiding (number, sign)
import Data.Functor.Identity (Identity)

type Unit = Float
type Vector = [Unit]
data Vertices = Vertices Vector
  deriving (Eq, Show)
data Vertex = Vertex Vector
  deriving (Eq, Show)
data Normal = Normal Vector
  deriving (Eq, Show)
data Facet = Facet Normal Vertices
  deriving (Eq, Show)
data Solid = Solid String [Facet]
  deriving (Eq, Show)

lexer :: Token.GenTokenParser String u Identity
lexer = Token.makeTokenParser Language.haskellDef
integer :: Parser Integer
integer = Token.integer lexer
float :: Parser Float
float = floating

sign :: Parser Unit
sign = option 1 $ do
  void $ try $ char '-'
  return (-1)

number :: Parser Unit
number = do
  sgn <- sign
  num  <- try float <|> orInt
  return $ num * sgn
  where orInt = fromInteger <$> integer

solid :: Parser Solid
solid = do
  skipMany space *> string "solid" *> skipMany1 space
  name   <- solidName
  facets <- many (skipMany space *> facet)
  void $ string "endsolid"
  return $ Solid name facets

solidName :: Parser String
solidName = do
  firstLetter <- letter
  restOfLetters <- many (alphaNum <|> char '_')
  return $ firstLetter : restOfLetters

facet :: Parser Facet
facet = do
  string "facet"    *> skipMany space
  string "normal"   *> skipMany space
  normal   <- Normal <$> vector
  string "outer"    *> skipMany space
  string "loop"     *> skipMany space
  vertices <- Vertices <$> vertex
  string "endloop"  *> skipMany space
  string "endfacet" *> skipMany space
  return $ Facet normal vertices

vertex :: Parser Vector
vertex = do
  [a, b, c] <- count 3 $ do
    string "vertex" *> skipMany space
    vector
  return $ a ++ b ++ c

vector :: Parser Vector
vector = do
  v <- count 3 $ do
    n <- number
    skipMany space
    return n
  return v

stlParse :: String -> Either ParseError Solid
stlParse = parse solid ""

stlParseFromFile :: FilePath -> IO (Either ParseError Solid)
stlParseFromFile = parseFromFile solid
