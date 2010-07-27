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

module Manual.Emit.XHTML
   (render_manual_xhtml)
   where

import Manual.Structure
import Text.Pretty

import Text.XHtml.Strict hiding (header,title,style)
import qualified Text.XHtml.Strict as X

import Data.Char

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
         (body $ concatHtml $ toHtml (header man) : toHtml (mcontents man) : map toHtml (sections man))

-- The header can be converted to html
instance HTML Header where
   toHtml head = concatHtml $
      (h1 (toHtml $ mtitle head) ! intro_ban_class (mtitle head)) : map (\b -> (h2 $ toHtml b) ! intro_ban_class b) (banners head) ++ map toHtml (preamble head)

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
         in  concatHtml $ replicate (3 * i) nbsp ++ [section_link sname unique, br]

section_title :: [Int] -> Banner -> String -> Html
section_title nums section unique = 
   (h2 $ (anchor $ stringToHtml (pretty_nums nums " ") +++ toHtml section) ! [name unique]) ! section_ban_class section

section_name :: [Int] -> String -> String 
section_name nums section = pretty_nums nums " " ++ section

-- A section can be converted to html
instance HTML Section where
   toHtml sec = concatHtml $
      section_title (number sec) (title sec) (unique sec) : map toHtml (stext sec) ++ map toHtml (subsections sec)

instance HTML Paragraph where
   toHtml para =
      paragraph (concatHtml $ map (html_inline (wrap para)) (ptext para)) ! [theclass $ pclass para]

-- Convert banners into html
instance HTML Banner where
   toHtml ban =
      concatHtml $ map (html_inline True) (btext ban)

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
html_inline :: Bool -> Inline -> Html
html_inline wrap inline = 
      case inline of
         IText str -> (if wrap then stringToHtml else literal_spaces . stringToHtmlString) str
         ISectionLink text dest -> section_link text dest
         IExternLink text dest  -> extern_link text dest
         ILiteral t -> primHtml t
         IClass t c -> thespan (stringToHtml t) ! [theclass c]

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
section_link :: String -> URL -> Html
section_link text dest = toHtml $ hotlink ('#':dest) (stringToHtml text)

-- | External link to something else
extern_link :: String -> URL -> Html
extern_link text dest = toHtml $ hotlink dest (stringToHtml text)
