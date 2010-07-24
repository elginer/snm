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

-- | Emit manuals
module Manual.Emit where

import Manual.Emit.XHTML
import Manual.Emit.Text
import Manual.Structure

import Text.Pretty

-- | Emit a manual as text
emit_text :: Manual -> String
emit_text = pretty

-- | Emit a manual as rendered xhtml
emit_xhtml :: Manual -> String
emit_xhtml = render_manual_xhtml 
