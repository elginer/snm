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

-- | Read and output manuals
module Manual where

import Manual.Reader
import Manual.Emit

import System.FilePath

-- | Create a text manual
create_text_manual :: FilePath -> IO String
create_text_manual =
   fmap emit_text . load_manual

-- | Write out a text file
write_txt_file :: FilePath -> String -> IO ()
write_txt_file filename txt =
   writeFile (filename `addExtension` "txt") txt 
