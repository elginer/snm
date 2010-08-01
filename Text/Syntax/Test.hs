{-

This file is part of The Simple Nice Manual Generator.

    The Simple Nice Manual Generator is free software: you can
    redistribute it and/or modify it under the terms of the GNU
    General Public License as published by the Free Software Foundation,
    either version 3 of the License, or any later version.

    The Simple Nice Manual Generator is distributed in the hope that it
    will be useful, but WITHOUT ANY WARRANTY; without even the implied
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with The Simple Nice Manual Generator.
    If not, see <http://www.gnu.org/licenses/>

-}

-- | A little module, exposed to test the syntax highlighting plugin system.
module Text.Syntax.Test where

import Manual.Structure

import Data.Dynamic
import Data.Either

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim

import Error.Report

import Control.Exception hiding (try)
import Control.Monad

-- | The plugin, highlights red, yellow, green, blue
colour :: Dynamic
colour = toDyn highlight_colour

-- | Highlight a colour string
highlight_colour :: SyntaxHighlighter
highlight_colour str =
   either perror id $ parse colour_or_plain "" str
   where
   perror e =
      throw $ error_lines ["Programmer error in colour plugin"
                          ,"in snm:Text.Syntax.Test"] $ report e

-- | Plain text or colour
colour_or_plain :: Parser (String, String, String)
colour_or_plain = plain_acc ""
   where
   plain_acc acc =
      try col_ahead <|> plain
      where
      col_ahead = do
         lookAhead polour
         if null acc
            then polour
            else do
               i <- getInput
               return ("", reverse acc, i)
      plain = do
         c <- anyChar
         plain_acc $ c : acc

-- | Colour parser
polour :: Parser (String, String, String)
polour = msum $ map try
   [col "red"
   ,col "yellow"
   ,col "green"
   ,col "blue"]

col :: String -> Parser (String, String, String)
col c = do
   string c
   i <- getInput
   return (c ++ "_elem", c, i)

