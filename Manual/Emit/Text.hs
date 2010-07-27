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

-- | Pretty print manuals as text
module Manual.Emit.Text where

import Manual.Structure

import Text.Pretty

import Data.List

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

mock_shows :: String -> ShowS
mock_shows s = (s ++) 

-- Prettify the section's number.
pretty_nums :: [Int] -> ShowS
pretty_nums nums = (intercalate "." (map show nums) ++) . ('.' :)

instance Pretty Paragraph where
   pretty' par _ =
      pretty_list' (ptext par) 0

-- | Copyright notice
copy_notice :: String -> String
copy_notice cp = "Copyright " ++ cp

-- | License notice
license_notice :: String -> String
license_notice lic = "Distributed under terms of the " ++ lic ++ "."

-- | License file notice
license_file_notice :: String -> String
license_file_notice fi = "See the file " ++ fi ++  " for details."

instance Pretty Header where
   pretty' head _ =
      pretty' (mtitle head) 0 . nl . nl . nl .
           pretty_list_nl' (banners head) 0 . nl . nl . nl .
           pretty_list_nl' (preamble head) 0

instance Pretty Banner where
   pretty' ban _ =
      pretty_list' (btext ban) 0

instance Pretty Inline where
   pretty' inline _ =
      case inline of
         IText str -> mock_shows str
         ISectionLink text dest -> mock_shows text
         IExternLink text dest -> mock_shows text . mock_shows " (see " . mock_shows dest . mock_shows ")"
         -- IIndent -> mock_shows "   "
         -- ILine -> nl
         ILiteral t -> mock_shows t
