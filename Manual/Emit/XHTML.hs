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

module Manual.Emit.XHTML where

import Manual.Structure
import Manual.Emit.Text

import Text.Pretty

import Text.XHtml.Strict hiding (copyright,header,title,style)
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
                           , thetitle $ stringToHtml $ mtitle $ header man
                           , X.style (primHtml $ style man) ! [thetype "text/css"]]) +++ 
         (body $ concatHtml $ toHtml (header man) : toHtml (mcontents man) : map toHtml (sections man))

-- The header can be converted to html
instance HTML Header where
   toHtml head = concatHtml $ map ((! [theclass "banner"]) . toHtml)
      [htitle $ mtitle head
      , hinfo $ copy_notice $ copyright head
      , hinfo $ license_notice $ license head
      , h2 $ concatHtml
         [stringToHtml "See the file " 
         ,(anchor (stringToHtml $ license_file head) ! [href $ license_file head])
         ,stringToHtml " for copying conditions."]] ++ map toHtml (preamble head)

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

htitle :: String -> Html
htitle = h1 . stringToHtml

hinfo :: String -> Html
hinfo = h2 . stringToHtml

section_title :: [Int] -> String -> String -> Html
section_title nums section unique = 
   h2 $ (anchor $ stringToHtml sname) ! [name unique]
   where
   sname = section_name nums section

section_name :: [Int] -> String -> String
section_name nums section = pretty_nums nums " " ++ section

-- A section can be converted to html
instance HTML Section where
   toHtml sec = concatHtml $
      section_title (number sec) (title sec) (unique sec) : map toHtml (stext sec) ++ map toHtml (subsections sec)

instance HTML Paragraph where
   toHtml para =
      paragraph (concatHtml $ ptext para) ! [theclass $ pclass para]

instance HTML Inline where
   toHtml inline =
      case inline of
         IText str -> stringToHtml $ str ++ " "
         ISectionLink text dest -> section_link text dest
         IExternLink text dest  -> extern_link text dest

-- | Internal link to elsewhere in the document
section_link :: String -> URL -> Html
section_link text dest = toHtml $ hotlink ('#':dest) (stringToHtml text)

-- | External link to something else
extern_link :: String -> URL -> Html
extern_link text dest = toHtml $ hotlink dest (stringToHtml text)
