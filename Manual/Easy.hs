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
module Manual.Easy
   (OutputType (..)
   ,load_manual
   ,write_manual
   ,formats
   ,text_format
   ,xhtml_format) where

import Manual.Reader
import Manual.Emit
import Manual.Structure

import System.FilePath

import Data.Map (Map)
import qualified Data.Map as M

-- | The output types
data OutputType =
     XHtml
   | Text
   deriving (Eq, Ord)


-- | Write in a number of formats
formats :: Map OutputType (Manual -> String, String)
formats = M.fromList [(XHtml, xhtml_format)
                     ,(Text, text_format)]

xhtml_format :: (Manual -> String, String)
xhtml_format = (emit_xhtml, "html")

text_format :: (Manual -> String, String)
text_format = (emit_text, "txt")

-- | Write out a manual
write_manual :: (Manual -> String) -> String -> Manual -> FilePath -> IO ()
write_manual format ext man fil =
   writeFile (fil `addExtension` ext) $ format man
