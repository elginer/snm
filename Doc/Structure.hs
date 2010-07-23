{-

Copyright 2010 John Morrice

This source file is part of Simple Nice Manual Generator and is distributed under the terms of the GNU General Public License

This file is part of Simple Nice Manual Generator.

    Simple Nice Manual Generator is free software: you can 
    redistribute it and/or modify it under the terms of the GNU 
    General Public License as published by the Free Software Foundation, 
    either version 3 of the License, or any later version.

    Simple Nice Manual Generator is distributed in the hope that it 
    will be useful, but WITHOUT ANY WARRANTY; without even the implied 
    warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
    See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Simple Nice Manual Generator.  
    If not, see <http://www.gnu.org/licenses/>

-}

-- | Generate manuals from a selection of source files
module Language.Obelisk.Report where

import System.FilePath
import System.Directory

-- | The Structure of the Obelisk report
data ObeliskReport = 

-- | Construct the obelisk report
write_obelisk_report :: FilePath -- ^ The directory the source files are in
                     -> FilePath -- ^ Where the output the report
                     -> IO ()    -- ^ Write the obelisk report
write_obelisk_report srcd out =
   read_report_files src >>= construct_report >>= writeFile out
