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
import Data.Maybe
import Data.List

import Error.Report

import Safe

import System.FilePath
import System.Directory

import qualified Data.Set as S

import Control.Monad
import Control.Exception hiding (try)


-- | Parse an inline element
inline :: Parser Inline
inline =
   try text <|> try section_link <|> try extern_link <|> try iliteral -- <|> try iindent <|>  iline
   where
   iliteral = do
      char '{'
      spaces
      string "literal"
      spaces
      text <- many1 $ noneOf "\n\t\r}"
      spaces
      char '}'
      return $ ILiteral text

{-   iindent = simple "indent" IIndent

   iline = simple "line" ILine

   simple id val = do
      char '{'
      spaces
      string id
      spaces
      char '}'
      return val -}

   text :: Parser Inline
   text = fmap (IText . concat) $ many1 itext
   itext :: Parser String
   itext =
      try (fmap return $ satisfy $ flip notElem "\\{")
         <|> (string "\\{" >> return "{")
   section_link = link "section" ISectionLink
   extern_link = link "external" IExternLink
   link name f = do
      char '{'
      spaces
      string name
      space
      spaces
      fst_elem <- many1 $ noneOf " \n\t\r"
      spaces
      rest <- sepEndBy1 end_id (space >> spaces)
      spaces
      char '}'
      let uniq = last rest
          txt = unwords $ fst_elem : init rest
      return $ f txt uniq
      where
      end_id = many1 $ noneOf " \n\t\r}"

-- | Parse inline elements
eparse_inline :: String -> Either ParseError [Inline]
eparse_inline txt =
   if null txt
      then Right [IText ""]
      else 
         parse (many1 inline) "" txt

-- | Parse inline elements
parse_inline :: String -> IO [Inline]
parse_inline txt = evaluate $
   either (throw . error_line "Error while parsing inline elements from paragraph beginning:" . error_section . error_line (take 10 txt) . error_section . (flip error_lines empty_error) . lines . show) 
          id
          (eparse_inline txt)

-- I say the orphan instances are okay because this is a module EXCLUSIVELY for reading in the manual

-- Convert a yaml description of a paragraph into a paragraph.
instance Yamlable Paragraph where
   from_yaml y =
      case y of
         YStr s ->
            liftM3 Paragraph (parse_inline s) (return "") (return True)
         YMap m -> let mw = yookup "wrap" y in do
            wrap <- maybe (return True) 
                          (evaluate . fromMaybe (perror "Error parsing wrap field.  Must be True or False.") .  
                             (\y -> ystr y >>= (readMay :: String -> Maybe Bool))) mw
            liftM3 Paragraph (parse_inline $ ptext "text") (evaluate $ ptext "class") (evaluate wrap)
         _      -> throw $ new_error "Paragraph must be a string or a map"
      where
      -- Look up paragraph text from a map
      ptext :: String -> String
      ptext nm =
         yext (yookup nm y) (perror $ "Could not find field '" ++ nm ++ "' in paragraph map.  Must have members 'text' and 'class'")

      perror msg = throw $ 
         error_line "Error while reading Paragraph: " $
            error_section $ error_line msg $ error_section $ 
               error_lines ["Reading yaml:", show y] $ empty_error

-- | Read a string from maybe a yaml
yext :: Maybe Yaml -> String -> String
yext y str = fromMaybe str $ y >>= ystr

ystr :: Yaml -> Maybe String
ystr y =
   case y of
      YStr s -> Just s
      _      -> Nothing

ymap :: (Yaml -> a) -> Yaml -> Maybe [a]
ymap f y =
   case y of
      YSeq ys -> Just $ map f ys
      _       -> Nothing

instance Yamlable Section where
   from_yaml y = do
      mps <- paras $ yookup "text" y
      ps <- maybe (serror "text") evaluate mps
      liftM5 Section (evaluate [snumber $ yookup "number" y])
              (evaluate $ stitle $ yookup "title" y)
              (evaluate $ read_key (serror "unique") $ yookup "unique" y)
              (return ps)
              (return [])
      where
      snumber a = 
         fromMaybe (serror "number")  $ a >>= ystr >>= readMay 

      stitle =
         read_key (serror "title")

      serror msg = throw $ 
         error_line "Error while reading Section:" $ error_section $
            error_line ("Section did not have a valid '" ++ msg ++ "' member.") $ error_section $
               error_lines ["Reading yaml:", show y] empty_error

read_key :: String -> Maybe Yaml -> String 
read_key err ma = not_empty err $ yext ma err

not_empty :: String -> String -> String
not_empty nm str =
   if null str
      then nm
      else str

-- | Load a section and all associated subsections.
load_section :: FilePath -> IO Section
load_section sfp = load_section_nums sfp [] 

-- | Catch errors while reading files
catch_read_errs :: String -> IO a -> IO a
catch_read_errs msg act =
   act `catches` [Handler reporth, Handler anyh]
   where
   reporth :: Error -> a
   reporth e = throw $ ewhere e
   ewhere = error_line msg . error_section
   anyh :: SomeException -> a
   anyh e = throw $ ewhere $ error_lines (lines $ show e) empty_error
   
-- | Load a section and all associated subsections.
load_section_nums :: FilePath
             -> [Int]
             -> IO Section
load_section_nums fp nums = catch_read_errs ("Error in section file " ++ fp) $ do
   root <- parse_yaml_file fp
   un_root_section <- from_yaml root
   let new_nums = number un_root_section ++ nums
       root_section = un_root_section {number = reverse new_nums}
       subsection_dir = dropExtensions fp
   subsections_exist <- doesDirectoryExist subsection_dir
   if subsections_exist
      then do
         subsect_fs <- getDirectoryFiles subsection_dir
         subsects <- mapM (flip load_section_nums new_nums) subsect_fs
         return $ root_section {subsections = sort_sections subsects}
      else
         evaluate root_section
   where
   dir = dropFileName fp

sort_sections :: [Section] -> [Section]
sort_sections = 
   sortBy (\s1 s2 -> number s1 `compare` number s2)

paras :: Maybe Yaml -> IO (Maybe [Paragraph])
paras ma =
   maybe (return Nothing) next ma
   where
   next :: Yaml -> IO (Maybe [Paragraph])
   next y =
      case y of
         YStr t -> fmap (Just . return) $ from_yaml y
         y -> maybe (return Nothing) (fmap Just . sequence) $ ymap from_yaml y

-- Load a header file
instance Yamlable Header where
   from_yaml y =
      liftM5 Header (evaluate title)
                    (evaluate copyright)
                    (evaluate license)
                    (evaluate license_file)
                    preamble
      where
      title = read_key (herror "title") $ yookup "title" y
      copyright = read_key (herror "copyright") $ yookup "copyright" y
      license = read_key (herror "license") $ yookup "license" y
      license_file = read_key (herror "license_file") $ yookup "license_file" y
      preamble = fmap (fromMaybe []) $ paras $ yookup "preamble" y
      herror nm = throw $ 
         error_line "Error while reading Header:" $
            error_section $ error_line ("Header did not have valid '" ++ nm ++ "' field.") $ error_section $ 
               error_lines ["Reading yaml:", show y] empty_error

-- | Get the qualified names of files in a directory
getDirectoryFiles :: FilePath -> IO [FilePath]
getDirectoryFiles dir = do
   fs <- fmap (map (combine dir) . filter (\f -> head f /= '.')) $ getDirectoryContents dir
   filterM doesFileExist fs

-- | Load a manual from a directory
load_manual :: FilePath -> IO Manual
load_manual man_dir = do
   fs <- getDirectoryFiles man_dir
   let files = S.fromList fs
       sfs = S.delete headf $ S.delete css_file $ files 
       headf = combine man_dir "header.yaml"
   catch_read_errs ("Error creating manual from source directory '" ++ man_dir ++ "':") (load_data headf files sfs)
   where
   load_data headf files sfs = do
      css <- if S.member css_file files then readFile css_file else return ""
      if S.member headf files
         then do
            head <- parse_yaml_file headf >>= from_yaml
            sections <- fmap sort_sections $ mapM load_section $ S.toList sfs
            evaluate $ Manual head css (contents sections) sections
         else throw $ new_error "Header not present"
   css_file = man_dir `combine` "style" `addExtension` "css"
