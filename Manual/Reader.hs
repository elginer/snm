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
module Manual.Reader
   (load_manual
   ,load_section
   ,eparse_inline) where

import Manual.Structure

import Data.Yaml.Simple

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Prim

import Data.Either
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Data.Char

import qualified Data.Map as M

import Data.Dynamic

import Error.Report

import Safe

import System.FilePath
import System.Directory
import System.Plugins

import Control.Monad
import Control.Exception hiding (try)

-- | Parse an inline element
inline :: Parser Inline
inline =
   try text <|> bracketed

bracketed :: Parser Inline   
bracketed = try section_link <|> try extern_link <|> try iliteral <|> try cls <|> ilang

ilang :: Parser Inline
ilang =
   inlines_element_attribute "language" ILanguage

cls :: Parser Inline
cls = inlines_element_attribute "class" IClass

iliteral :: Parser Inline
iliteral = string_element "literal" ILiteral

string_element :: String -> (String -> Inline) -> Parser Inline
string_element nm f = inline_element nm $ do
   text <- many1 $ noneOf "}"
   return $ f text

inline_element :: String -> Parser Inline -> Parser Inline
inline_element nm ma = do
      char '{'
      spaces
      string nm
      space
      spaces
      a <- ma
      spaces
      char '}'
      return a

text :: Parser Inline
text = fmap IText $
   try (many1 $ noneOf "\\{") <|> 
      try (string "\\{" >> return "{") <|>
         (fmap return $ char '\\')

section_link = inlines_element_attribute  "section" ISectionLink
extern_link = inlines_element_attribute "external" IExternLink

nested :: Parser Inline
nested = try bracketed <|> try (do
   s <- many1 $ noneOf " {\n\r\t}"
   return $ IText $ ' ' : s)

chomp :: String -> String
chomp = reverse . dropWhile isSpace . reverse . dropWhile isSpace

inlines_element_attribute :: String -> ([Inline] -> String -> Inline) -> Parser Inline 
inlines_element_attribute name f = inline_element name $ do
   fst_elem <- nested
   space
   spaces
   rest <- sepEndBy1 nested  
                     (space >> spaces)
   let uniqi = last rest
       txt = fst_elem : init rest
      -- This is sooo much of a hack because there isn't a decent grammar.
   uniq <- case uniqi of
              IText t -> return $ chomp t
              _       -> unexpected "nested inline element: the last element should be a string."
   return $ f txt uniq

-- | Parse inline elements
eparse_inline :: String -> Either ParseError [Inline]
eparse_inline txt =
   if null txt
      then Right [IText ""]
      else 
         parse (do
            is <- many1 inline
            eof
            return is) "" txt

-- | Parse inline elements
parse_inline :: String -> IO [Inline]
parse_inline txt = evaluate $
   either (throw . error_line "Error while parsing inline elements from paragraph beginning:" . error_section . 
             error_line (unwords (take 5 $ words txt) ++ "...") . error_section . (flip error_lines empty_error) . lines . show) 
          id
          (eparse_inline txt)

-- I say the orphan instances are okay because this is a module EXCLUSIVELY for reading in the manual

-- Read a banner from a yaml description
instance Yamlable Banner where
   from_yaml y =
      case y of
         YStr s -> liftM2 Banner (parse_inline s) (return "") 
         YMap _ -> let mcls = yookup "class" y in do
            clss <- maybe (return "")
                          (evaluate . fromMaybe (berror "Error parsing class field.  Must be a string.") . ystr) mcls
            txt <- maybe (return [])
                         (maybe (evaluate $ berror "Error parsing text field.") parse_inline . ystr) $ yookup "text" y

            when (null txt) $ evaluate $ berror "Banner text cannot be empty"
            evaluate $ Banner txt clss
         _      -> berror "Banner must be a string or a map."
      where
      berror msg = throw $ 
         error_line "Error while reading Banner: " $
            error_section $ error_line msg $ error_section $ 
               error_lines ["Reading yaml:", show y] $ empty_error


-- Convert a yaml description of a paragraph into a paragraph.
instance Yamlable Paragraph where
   from_yaml y =
      case y of
         YStr s ->
            liftM4 Paragraph (parse_inline s) (return "") (return "") (return True)
         YMap m -> let mw = yookup "wrap" y 
                       mcls = yookup "class" y 
                       mlang = yookup "language" y in do
           
            wrap <- maybe (return True) 
                         (evaluate . fromMaybe (perror "Error parsing wrap field.  Must be True or False.") .  
                             (\y -> ystr y >>= (readMay :: String -> Maybe Bool))) mw
            
            clss <- maybe (return "")
                          (evaluate . fromMaybe (perror "Error parsing class field.  Must be a string.") . ystr) mcls
            
            lang <- maybe (return "")
                          (evaluate . maybe (perror "Error parsing language field.  Must be a string.") chomp . ystr) mlang

            liftM4 Paragraph (parse_inline $ ptext "text") (evaluate clss) (evaluate lang) (evaluate wrap)
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

-- | Try to read a string from yaml
ystr :: Yaml -> Maybe String
ystr y =
   case y of
      YStr s -> Just s
      _      -> Nothing

-- | Try to map with yaml
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
              (stitle $ yookup "title" y)
              (evaluate $ read_key (serror "unique") $ yookup "unique" y)
              (return ps)
              (return [])
      where
      snumber a = 
         fromMaybe (serror "number")  $ a >>= ystr >>= readMay 

      stitle =
         read_mbanner (serror "title")

      serror msg = throw $ 
         error_line "Error while reading Section:" $ error_section $
            error_line ("Section did not have a valid '" ++ msg ++ "' member.") $ error_section $
               error_lines ["Reading yaml:", show y] empty_error

-- | Read a banner from what might be some yaml
read_mbanner :: Banner -> Maybe Yaml -> IO Banner
read_mbanner ba =
   maybe (evaluate ba) from_yaml

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

-- Load the manual environment
instance Yamlable ManEnv where
   from_yaml y = 
      case y of
         YSeq syn_ys -> fmap (ManEnv . M.fromList) $ mapM load_syntax_highlighter syn_ys
         YMap _ -> fmap (ManEnv . uncurry M.singleton) $ load_syntax_highlighter y
         _ -> 
           env_err "Expected map or sequence at the top level."
      where
      env_err msg =
         throw $ error_line "Error while reading manual environment:" $ 
            error_section $ error_line msg $ empty_error
      -- Read the values in the IO monad so we're sure the errors are triggered immediately
      load_syntax_highlighter :: Yaml -> IO (String, SyntaxHighlighter)
      load_syntax_highlighter y = do
         slang <- fmap chomp $ eookup "language" y
         spkg <- eookup "package" y
         smod <- eookup "module" y
         sym <- eookup "symbol" y
         md <- loadDynamic (spkg, smod, sym)
         d <- evaluate $ fromMaybe (env_err $ "Could not load syntax highlighter: " ++ spkg ++ ":" ++ smod ++ "." ++ sym) $ 
                 md >>= fromDynamic
         return (slang, (d :: SyntaxHighlighter))
      eookup nm y = evaluate $
         fromMaybe (env_err $ "Invalid field: " ++ nm) $ yookup nm y >>= ystr

-- Load a header file
instance Yamlable Header where
   from_yaml y = let mb = yookup "banners" y in do
      bs <- maybe (return []) (\ybs ->
         case ybs of
            YStr _ -> fmap return $ from_yaml ybs
            YSeq ys -> mapM from_yaml ys
            YMap _   -> fmap return $ from_yaml ybs
            _       -> herror "banner") mb
      liftM3 Header title
                    (evaluate bs)
                    preamble
      where
      title = read_mbanner (herror "title") $ yookup "title" y
      preamble = fmap (fromMaybe []) $ paras $ yookup "preamble" y
      herror nm = throw $ 
         error_line "Error while reading Header:" $
            error_section $ error_line ("Header did not have valid '" ++ nm ++ "' field.") $ error_section $               empty_error

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
   catch_read_errs ("Error creating manual from source directory '" ++ man_dir ++ "':") $ do
      header <- load_header files
      css <- load_css files
      syntax <- load_syntax files
      sections <- load_sections files
      return $ Manual header css syntax (contents sections) sections
   where
   -- Load the sections
   load_sections files = 
      fmap sort_sections $ mapM load_section $ S.toList sfs 
      where
      sfs = S.delete syntaxf $ S.delete headf $ S.delete css_file $ files  

   -- Load the manual header
   load_header files =
      if S.member headf files
         then
            parse_yaml_file headf >>= from_yaml
         else 
            throw $ new_error "Header not present"

   headf = man_dir `combine` "header" `addExtension` "yaml"

   -- Load the syntax file
   load_syntax files =
      if S.member syntaxf files
         then parse_yaml_file syntaxf >>= from_yaml
         else return $ ManEnv $ M.empty
   
   syntaxf = man_dir `combine` "syntax" `addExtension` "yaml"

   -- Load the css
   load_css files = 
      if S.member css_file files 
         then readFile css_file 
         else return ""

   css_file = 
      man_dir `combine` "style" `addExtension` "css"


