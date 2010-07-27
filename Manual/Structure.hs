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
    along with The Simple Nice Manual Generator.  
    If not, see <http://www.gnu.org/licenses/>

-}

-- | The structure of a manual
module Manual.Structure
   (Manual (..)
   ,Header (..)
   ,Contents (..)
   ,Section (..)
   ,Banner (..)
   ,Paragraph (..)
   ,Inline (..)
   ,pretty_nums
   ,contents)
   where

import System.FilePath
import System.Directory

import Data.List

import Text.Pretty

-- | We just see a CSS file as a string
type CSS = String

-- | A banner for the top of the document
data Banner = Banner 
   {btext :: ![Inline]
   ,bclass :: !String}
   deriving Show

-- | A manual's header file
data Header = Header 
   { -- | The name of the manual
     mtitle :: !Banner
   , -- | Banners
     banners :: ![Banner]
   , -- | Preamble, not linked in the manual's contents.
     preamble :: ![Paragraph] 
   }
   deriving Show

-- | Inline document elements may be...
data Inline =
     -- | A text string
     IText String
   | -- | A link to another section
     ISectionLink
     { -- | The link text.
       ltext :: String
     , -- | The section number
       ldest :: String 
     }
   | -- | An external link.
     IExternLink
     { -- | The link text.
       ltext :: String
     , -- | The link destination.
       ldest :: String
     }
   | -- | Literal text
     ILiteral String
   | -- | Italics
     IItalic String
   | -- | Inline class
     IClass String String
   deriving Show

-- | A text paragraph
data Paragraph = Paragraph
   { -- | The paragraph text
     ptext :: ![Inline]
   , -- | The paragraph's class
     pclass :: String
   , -- | Wrap the text
     wrap :: Bool
   } 
   deriving Show

-- | A manual
data Manual = Manual
   { -- | The manual's header file.
     header :: Header
   , -- | The style used in the manual.
     style  :: CSS
   , -- | The manual's contents.
     mcontents :: Contents
   , -- | The sections of a manual.
     sections :: ![Section]
   }
   deriving Show

-- | Sections, subsections
data Section = Section
   { -- | The section's number.
     number :: [Int]
   , -- | The title of the section
    title :: Banner 
   , -- | Unique name for this section
    unique :: String
   , -- | The section text
    stext :: ![Paragraph]
     -- | Subsections
   , subsections :: ![Section]
   }
   deriving Show

-- | Contents listing
data Contents = 
   Contents [Contents]
   | Entry [Int] String String
   deriving Show

-- | Create the manual's contents
contents :: [Section] -> Contents
contents =
   Contents . gather_sections

gather_sections :: [Section] -> [Contents]
gather_sections ss = 
   concatMap subsection_contents $ 
      sortBy (\s1 s2 -> number s1 `compare` number s2) ss

subsection_contents :: Section -> [Contents]
subsection_contents s =
   if null $ subsections s
      then [me]
      else [me , Contents $ gather_sections (subsections s)]
   where
   me = Entry (number s) (pretty $ title s) (unique s)

instance Pretty Banner where
   pretty' ban _ =
      pretty_list' (btext ban) 0

instance Pretty Inline where
   pretty' inline _ =
      case inline of
         IText str -> mock_shows str
         ISectionLink text dest -> mock_shows text
         IExternLink text dest -> mock_shows text . mock_shows " (see " . mock_shows dest . mock_shows ")"
         ILiteral t -> mock_shows t
         IItalic t  -> mock_shows t
         IClass t _  -> mock_shows t

mock_shows :: String -> ShowS
mock_shows s = (s ++) 

instance Pretty Contents where
   pretty' c sp =
      case c of
         Entry nums name _ -> pspace sp . pretty_nums nums . ((' ' : name) ++)
         Contents cs -> pretty_list' cs $ sp + 1

-- Pretty print manuals
instance Pretty Manual where
   pretty' man _ =
      pretty' (header man) 0 . nl . nl . 
         pretty' (mcontents man) (-1) . nl . nl . nl .
            pretty_list' (sections man) 0

instance Pretty Section where
   pretty' sec _ =
      pretty_nums (number sec) . (' ' :) . pretty' (title sec) 0 . nl . nl . 
         pretty_list_nl' (stext sec) 0 . nl . nl . nl .
            (if null $ subsections sec
               then id
               else pretty_list' (subsections sec) 0)

-- Prettify the section's number.
pretty_nums :: [Int] -> ShowS
pretty_nums nums = (intercalate "." (map show nums) ++) . ('.' :)

instance Pretty Paragraph where
   pretty' par _ =
      pretty_list' (ptext par) 0

instance Pretty Header where
   pretty' head _ =
      pretty' (mtitle head) 0 . nl . nl . nl .
           pretty_list_nl' (banners head) 0 . nl . nl . nl .
           pretty_list_nl' (preamble head) 0
