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

-- | Create simple syntax highlighting plugins
module Text.Syntax.Simple where

import Manual.Structure

import Data.Dynamic
import Data.Either

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim

import Error.Report

import Control.Exception hiding (try)
import Control.Monad

-- | Create a simple plugin which highlights keywords.
highlight :: String -- ^ The name of the plugin
          -> [(String, String, String)] -- ^ The first is the keyword, second the class, third the returned string (allowing the keyword to be transformed)
       -> Dynamic
highlight name = toDyn . associate_keywords name

-- | Associate keywords with a class
associate_keywords :: String -- ^ The name of the plugin
                   -> [(String, String, String)] -- ^ The first is the keyword, second the class, third the returned string (allowing the keyword to be transformed)
                   -> SyntaxHighlighter
associate_keywords name key_assoc str =
   either perror id $ parse (keyword_or_plain key_assoc) "" str
   where
   perror e =
      throw $ error_lines ["Programmer error in plugin '" ++ name ++ "'"] $ report e

-- | Plain text or highlight
keyword_or_plain :: [(String, String, String)] -- ^ The first is the keyword, second the class, third the returned string (allowing the keyword to be transformed)
                -> Parser (String, String, String)
keyword_or_plain key_assoc = plain_acc ""
   where
   plain_acc acc =
      try key_ahead <|> plain
      where
      key_ahead = do
         lookAhead $ grab_keys key_assoc
         if null acc
            then grab_keys key_assoc
            else do
               i <- getInput
               return ("", reverse acc, i)
      plain = do
         c <- anyChar
         plain_acc $ c : acc

-- | Parse one of many keyworsd
grab_keys :: [(String, String, String)] -- ^ The first is the keyword, second the class, third the returned string (allowing the keyword to be transformed)

          -> Parser (String, String, String)
grab_keys = msum  .  map (try  .  grab_key)

-- | Parse one keyword
grab_key :: (String, String, String) -- ^ The first is the keyword, second the class, third the returned string (allowing the keyword to be transformed)
         -> Parser (String, String, String)
grab_key (keyword, cls, ret)  = do
   string keyword
   i <- getInput
   return (cls, ret, i)

