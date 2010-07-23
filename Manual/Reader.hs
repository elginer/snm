{-

Copyright 2010 John Morrice

This source file is part of The Simple Nice Manual Generator and is distributed under the terms of the GNU General Public License

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
    along with The Simple Nice Manual enerator.  
    If not, see <http://www.gnu.org/licenses/>

-}

-- | Read a manual from its source files
module Manual.Reader where

import Manual.Structure

import Data.Yaml.Simple

import Text.Parsec
import Text.Parsec.String

import Data.Either

-- | Parse an inline element
inline :: Parser Inline
inline =
   try text <|> try section_link <|> extern_link
   where
   text :: Parser Inline
   text = fmap (IText . concat) $ many1 itext
   itext :: Parser String
   itext =
      try (fmap return $ satisfy $ flip notElem "\\{")
         <|> string "\\{"
   section_link = link "section" ISectionLink
   extern_link = link "external" IExternLink
   link name f = do
      char '{'
      spaces
      string name
      space
      spaces
      txt <- many1 $ noneOf " \n\t\r"
      spaces
      uniq <- many1 $ noneOf " \n\t\r}"
      spaces
      char '}'
      return $ f txt uniq

-- | Parse inline elements
parse_inline :: String -> [Inline]
parse_inline txt =
   if null txt
      then [IText ""]
      else 
         either (error . show) id $ parse (many1 inline) "" txt

-- Convert a yaml description of a paragraph into a paragraph.
instance Yamlable Paragraph where
   from_yaml y =
      case y of
         YStr s ->
            Paragraph (parse_inline s) ""
         YMap m ->
            Paragraph (parse_inline $ ptext "text") (ptext "class")
         _      -> perror "Paragraph cannot be a series."
      where
      -- Look up paragraph text from a map
      ptext :: String -> String
      ptext nm =
         case yookup nm y of
            Just (YStr s) -> s
            _      -> perror $ "Could not find text field '" ++ nm ++ "' in paragraph map.  Must have members 'txt' and 'class'"
      perror msg = error $ "Error while reading paragraph: " ++ msg
