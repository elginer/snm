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

import Text.Syntax.Simple

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
colour = highlight "colour" $ map (\c -> (c, c ++ "_elem", c)) ["red", "yellow", "green", "blue"]

