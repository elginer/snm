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

{-#
   OPTIONS
   -XFlexibleInstances
   -XFlexibleContexts
#-}

module Manual.Emit.XHTML
   (render_manual_xhtml)
   where

import Manual.Structure
import Text.Pretty

import Text.XHtml.Strict hiding (header,title,style)
import qualified Text.XHtml.Strict as X

import Data.Char
import qualified Data.Map as M
import Data.List

-- | Render the manual as XHTML and show as a string
render_manual_xhtml :: Manual -> String
render_manual_xhtml = showHtml . toHtml

-- The manual can be converted to html
instance HTML Manual where
   toHtml man =
      X.header (concatHtml [meta ! [httpequiv "Content-Type"
                                   ,content "text/html;charset=utf-8"]
                           , thetitle $ stringToHtml $ pretty $ mtitle $ header man
                           , X.style (primHtml $ style man) ! [thetype "text/css"]]) +++ 
         (body $ concatHtml $ toHtml (header man, man_env man) : toHtml (mcontents man) : map_env (sections man) (man_env man))

-- The header can be converted to html
instance HTML (Header,ManEnv) where
   toHtml (head, man_env) = concatHtml $
      (h1 (toHtml $ mtitle head) ! intro_ban_class (mtitle head)) : 
         map (\b -> (h2 $ toHtml b) ! intro_ban_class b) (banners head) ++ 
            map_env (preamble head) man_env

-- | Get the banner class for banners in the introduction
intro_ban_class :: Banner -> [HtmlAttr]
intro_ban_class = ban_class "intro_banner"

-- The contents
instance HTML Contents where
   toHtml = paragraph . hcontents (-1)

nbsp :: Html
nbsp = spaceHtml

hcontents :: Int -> Contents -> Html
hcontents i c =
   case c of
      Contents cs -> concatHtml $ map (hcontents $ i + 1) cs
      Entry nums str unique ->
         let sname = section_name nums str
         in  concatHtml $ replicate (3 * i) nbsp ++ [section_link (stringToHtml sname) unique, br]

section_title :: [Int] -> Banner -> String -> Html
section_title nums section unique = 
   (h2 $ (anchor $ stringToHtml (pretty_nums nums " ") +++ toHtml section) ! [name unique]) ! section_ban_class section

section_name :: [Int] -> String -> String 
section_name nums section = pretty_nums nums " " ++ section

-- | Map to html with a manual environment
map_env :: HTML (x, ManEnv) => [x] -> ManEnv -> [Html]
map_env ms man_env = 
   map toHtml $ zip ms (repeat man_env)

-- A section can be converted to html
instance HTML (Section, ManEnv) where
   toHtml (sec, man_env) = concatHtml $
      section_title (number sec) (title sec) (unique sec) : 
         map_env (stext sec) man_env ++ map_env (subsections sec) man_env

instance HTML (Paragraph,ManEnv) where
   toHtml (para, man_env) =
      paragraph (concatHtml $ paragraph_htmls) ! [theclass $ pclass para]
      where
      paragraph_htmls = 
         map (find_syntax_highlight man_env (language para) (wrap para)) (ptext para)


-- | Find the syntax and highlight
find_syntax_highlight :: ManEnv -- ^ The environment
                      -> String -- ^ The language
                      -> Bool   -- ^ Wrap the text
                      -> Inline -- ^ Inline element to render
                      -> Html   -- ^ Render html
find_syntax_highlight man_env language wrap inline = flip ($) inline $
   if null language
      then without_lang
      else
         maybe without_lang with_lang $ 
            M.lookup language (syntax man_env)
   where
   with_lang trans il =
      highlight wrap il trans man_env
   without_lang il = 
      html_inline wrap il man_env

-- | Highlight xhtml
highlight :: Bool -- ^ Wrap the text
          -> Inline -- ^ The inline element
          -> SyntaxHighlighter -- ^ The syntax highlighter
          -> ManEnv -- ^ The manual environment 
          -> Html -- ^ Produce Html
highlight wrap inline syn man_env =
      case inline of
         IText str -> highlight_syntax str
         ISectionLink text dest -> section_link (concatHtml $ map recurse text) dest
         IExternLink text dest  -> extern_link (concatHtml $ map recurse text) dest
         ILiteral t -> primHtml t
         IClass t c -> thespan (concatHtml $ map recurse t) ! [theclass c]
         ILanguage t lang -> concatHtml $ map (find_syntax_highlight man_env lang wrap) t
   where
   highlight_syntax str =
      concatHtml $ map (uncurry create_syntax_element) $ unfoldr syntax_class str
   syntax_class text =
      if null text
         then Nothing
         else Just ((consumed, cls), rest)
      where
      (cls, consumed, rest) = syn text
   create_syntax_element text cls =
      if null cls
         then trans_text
         else (thespan $ trans_text) ! [theclass cls]
      where
      trans_text = 
         (if wrap 
            then primHtml
            else literal_spaces) $ stringToHtmlString text
   recurse il = 
      highlight wrap il syn man_env

          

-- Convert banners into html
instance HTML Banner where
   toHtml ban =
      concatHtml $ map (\il -> html_inline True il new_man_env) (btext ban)

-- | Banners for the sections
section_ban_class :: Banner -> [HtmlAttr]
section_ban_class = ban_class "banner"

-- | The banner's class
ban_class :: String -> Banner -> [HtmlAttr]
ban_class def ban = [theclass cls]
   where
   cls = 
      if null $ bclass ban
         then def
         else bclass ban      

-- | Make an inline element into html   
html_inline :: Bool -- ^ Wrap the text
            -> Inline -- ^ The inline element
            -> ManEnv -- ^ The manual's environment
            -> Html -- ^ Produced html
html_inline wrap inline man_env = 
      case inline of
         IText str -> (if wrap then stringToHtml else literal_spaces . stringToHtmlString) str
         ISectionLink text dest -> section_link (concatHtml $ map recurse text) dest
         IExternLink text dest  -> extern_link (concatHtml $ map recurse text) dest
         ILiteral t -> primHtml t
         IClass t c -> thespan (concatHtml $ map recurse t) ! [theclass c]
         ILanguage t lang -> concatHtml $ map (find_syntax_highlight man_env lang wrap) t
   where
   recurse il = html_inline wrap il man_env

-- | Convert all spaces and newlines into literal html spaces and newlines.
literal_spaces :: String -> Html
literal_spaces = concatHtml . map literal_space
   where
   literal_space c =
      case c of
         ' ' -> nbsp 
         '\t' -> concatHtml $ replicate 6 nbsp
         '\n' -> br
         _    -> primHtml [c]
         

-- | Internal link to elsewhere in the document
section_link :: Html -> URL -> Html
section_link text dest = toHtml $ hotlink ('#':dest) text

-- | External link to something else
extern_link :: Html -> URL -> Html
extern_link text dest = toHtml $ hotlink dest text
