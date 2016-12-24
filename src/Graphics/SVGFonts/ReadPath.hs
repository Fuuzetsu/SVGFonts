{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}

--------------------------------------------------------------------
-- |
-- Module    : Graphics.SVG.ReadPath
-- Copyright : (c) 2011 Tillmann Vogt
-- License   : BSD3
--
-- Maintainer: Tillmann Vogt <tillk.vogt@googlemail.com>
-- Stability : stable
-- Portability: portable
--
-- Parsing the SVG path command, see <http://www.w3.org/TR/SVG/paths.html#PathData> :

module Graphics.SVGFonts.ReadPath
 ( pathFromByteString,
   PathCommand(..),
 )
 where

import Control.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString.Char8 (Parser, many1)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

data PathCommand n =
  M_abs !(n, n) | -- ^Establish a new current point (with absolute coords)
  M_rel !(n, n) | -- ^Establish a new current point (with coords relative to the current point)
  Z | -- ^Close current subpath by drawing a straight line from current point to current subpath's initial point
  L_abs !(n, n) | -- ^A line from the current point to (n, n) which becomes the new current point
  L_rel !(n, n) |
  H_abs !n | -- ^A horizontal line from the current point (cpx, cpy) to (x, cpy)
  H_rel !n |
  V_abs !n | -- ^A vertical line from the current point (cpx, cpy) to (cpx, y)
  V_rel !n |
  C_abs !(n,n,n,n,n,n) | -- ^Draws a cubic Bézier curve from the current point to (x,y) using (x1,y1) as the
  -- ^control point at the beginning of the curve and (x2,y2) as the control point at the end of the curve.
  C_rel !(n,n,n,n,n,n) |
  S_abs !(n,n,n,n) | -- ^Draws a cubic Bézier curve from the current point to (x,y). The first control point is
-- assumed to be the reflection of the second control point on the previous command relative to the current point.
-- (If there is no previous command or if the previous command was not an C, c, S or s, assume the first control
-- point is coincident with the current point.) (x2,y2) is the second control point (i.e., the control point at
-- the end of the curve).
  S_rel !(n,n,n,n) |
  Q_abs !(n,n,n,n) | -- ^A quadr. Bézier curve from the curr. point to (x,y) using (x1,y1) as the control point
  Q_rel !(n,n,n,n) | -- ^Nearly the same as cubic, but with one point less
  T_abs !(n, n) | -- ^T_Abs = Shorthand/smooth quadratic Bezier curveto
  T_rel !(n, n) |
  A_abs | -- ^A = Elliptic arc (not used)
  A_rel
  deriving (Show, Functor)

pathFromByteString :: Fractional n => ByteString -> Either String [PathCommand n]
pathFromByteString str = case P.parseOnly path str of
  Left  err -> Left err
  Right p   -> Right $! fmap realToFrac <$> p

spaces :: Parser ()
spaces = P.skipSpace

path :: Parser [PathCommand Double]
path = do
  l <- many pathElement
  P.endOfInput
  return $! concat l

pathElement :: Parser [PathCommand Double]
pathElement =
  P.skipSpace *>
  (  symbol 'M' *> many1 (M_abs <$> tupel2)
 <|> symbol 'm' *> many1 (M_rel <$> tupel2)
 <|> symbol 'z' *> pure [Z]
 <|> symbol 'Z' *> pure [Z]
 <|> symbol 'L' *> many1 (L_abs <$> tupel2)
 <|> symbol 'l' *> many1 (L_rel <$> tupel2)
 <|> symbol 'H' *> many1 (H_abs <$> P.double)
 <|> symbol 'h' *> many1 (H_rel <$> P.double)
 <|> symbol 'V' *> many1 (V_abs <$> P.double)
 <|> symbol 'v' *> many1 (V_rel <$> P.double)
 <|> symbol 'C' *> many1 (C_abs <$> tupel6)
 <|> symbol 'c' *> many1 (C_rel <$> tupel6)
 <|> symbol 'S' *> many1 (S_abs <$> tupel4)
 <|> symbol 's' *> many1 (S_rel <$> tupel4)
 <|> symbol 'Q' *> many1 (Q_abs <$> tupel4)
 <|> symbol 'q' *> many1 (Q_rel <$> tupel4)
 <|> symbol 'T' *> many1 (T_abs <$> tupel2)
 <|> symbol 't' *> many1 (T_rel <$> tupel2)
 <|> symbol 'A' *> many1 (A_abs <$  tupel2)
 <|> symbol 'a' *> many1 (A_rel <$  tupel2)
  )

comma :: Parser ()
comma = P.skipSpace *> (symbol ',' <|> pure ())

tupel2 :: Parser (Double, Double)
tupel2 = do
  x <- P.double
  comma
  y <- P.double
  spaces
  return (x, y)

tupel4 :: Parser (Double,Double,Double,Double)
tupel4 = do{ x1 <- P.double; comma; y1 <- P.double; spaces;
              x <- P.double; comma;  y <- P.double; spaces;
             return (x1, y1, x, y)
           }

tupel6 :: Parser (Double,Double,Double,Double,Double,Double)
tupel6 = do{ x1 <- P.double; comma; y1 <- P.double; spaces;
             x2 <- P.double; comma; y2 <- P.double; spaces;
              x <- P.double; comma;  y <- P.double; spaces;
             return (x1, y1, x2, y2, x, y)
           }

symbol :: Char -> Parser ()
symbol c = P.string (BS.singleton c) *> P.skipSpace
