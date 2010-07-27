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
module Manual.Structure where

import System.FilePath
import System.Directory

import Data.List

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
