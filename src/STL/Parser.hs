{-# LANGUAGE OverloadedStrings #-}

module STL.Parser where
import Text.Parsec
import Text.Parsec.String (Parser, parseFromFile, GenParser)
import Text.Parsec.Combinator
import qualified Text.Parsec.Language as Language
import qualified Text.Parsec.Token    as Token
import Control.Monad (void, guard)
import System.Environment
import Unsafe.Coerce
import GHC.Float
import qualified Data.Vector.Storable as V
import Text.Parsec.Number hiding (number, sign)

type Unit = Float
type Vector = V.Vector Unit
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

lexer = Token.makeTokenParser Language.haskellDef
lexeme = Token.lexeme lexer
integer = Token.integer lexer
float = floating

sign :: Parser Unit
sign = option 1 $ do
  try $ char '-'
  return (-1)

number :: Parser Unit
number = do
  sgn <- sign
  num  <- try float <|> int
  return $ num * sgn
  where int = fromInteger <$> integer

solid :: Parser Solid
solid = do
  skipMany space *> string "solid" *> skipMany1 space
  name   <- solidName
  facets <- many (skipMany space *> facet)
  string "endsolid"
  return $ Solid name facets

solidName = do
  firstLetter <- letter
  restOfLetters <- many alphaNum
  return $ firstLetter : restOfLetters

facet :: Parser Facet
facet = do
  string "facet"    *> skipMany space
  string "normal"   *> skipMany space
  normal   <- Normal <$> V.fromList <$> vector
  string "outer"    *> skipMany space
  string "loop"     *> skipMany space
  vertices <- Vertices <$> vertex
  string "endloop"  *> skipMany space
  string "endfacet" *> skipMany space
  return $ Facet normal vertices

vertex = do
  v@[a, b, c] <- count 3 $ do
    string "vertex" *> skipMany space
    vector
  return $ V.fromList $ a ++ b ++ c

vector :: Parser [Unit]
vector = do
  v@[a, b, c] <- count 3 $ do
    n <- number
    skipMany space
    return n
  return v

stlParse = parse solid ""
stlParseFromFile = parseFromFile solid
