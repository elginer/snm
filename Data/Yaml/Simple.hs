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

-- | Simpler yaml interface
module Data.Yaml.Simple where

import Data.Yaml.Syck (YamlNode (..), YamlElem (..), unpackBuf)

import qualified Data.Yaml.Syck as S

import qualified Data.Map as M
import Data.Map (Map)

import Control.Arrow

class Yamlable y where
   from_yaml :: Yaml -> IO y

-- | Yaml
data Yaml =
   YMap (Map Yaml Yaml)
   | YSeq [Yaml]
   | YStr String
   | YNil
   deriving (Show, Eq, Ord)

-- | Simplify Syck's yaml type
simplify :: YamlNode -> Yaml
simplify yaml = 
   case n_elem yaml of
      EMap l -> YMap $ M.fromList $ map (simplify *** simplify) l
      ESeq l -> YSeq $ map simplify l
      EStr b -> YStr $ unpackBuf b
      ENil   -> YNil

-- | Parse yaml
parse_yaml :: String -> IO Yaml
parse_yaml = fmap simplify . S.parseYaml

-- | Parse yaml file
parse_yaml_file :: FilePath -> IO Yaml
parse_yaml_file = fmap simplify . S.parseYamlFile

-- | Look up a yaml element referenced by a string in a yaml map
yookup :: String -> Yaml -> Maybe Yaml
yookup key yaml =
   case yaml of
      YMap m -> M.lookup (YStr key) m
      _      -> Nothing
