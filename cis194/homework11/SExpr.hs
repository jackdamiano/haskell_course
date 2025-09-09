{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

-- Using local AParser
import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

-- These two functions together look like the recursive definition for mapA in Sandbox11, just parser specific and allows for multiple functionality checking

-- :: f a -> f [a]
-- Never fails. If oneOrMore fails (meaning 0), just return the empty list in Parser context (pure)
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- lifts the parser into list land, repeats for as long as Parser lets it using zeroOrMore (which returns empty Parser [] or ANOTHER successfully parsed value)
-- If the first fails, it will just return the fail condition since we're evaluating p no matter what
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

-- these two functions together is like a mapParser but you can interject whether you want
-- a fail state or an empty state for the default case depending on the function call. Very cool!

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
--ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)
ident = liftA2 (++) (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show


atoms :: Parser Atom
atoms = (N <$> posInt) <|> (I <$> ident)

-- get rid of spaces at front, check if Atom or parenthesis/comb. if Atom, process using parse,
-- if (x), discard the inner and outer parentheses and recursively process the expression using zeroOrMore.
-- Once complete, get rid of white space at end as well
parseSExpr :: Parser SExpr
parseSExpr = spaces *> ((A <$> atoms) <|> (char '(' *> (Comb <$> zeroOrMore parseSExpr) <* char ')')) <* spaces