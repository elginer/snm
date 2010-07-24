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

import Text.XHtml.Strict hiding (copyright,header,title)

-- | Render the manual as XHTML and show as a string
render_manual_xhtml :: Manual -> String
render_manual_xhtml = showHtml . toHtml

-- The manual can be converted to html
instance HTML Manual where
   toHtml man =
      thehtml $ body $ concatHtml $ toHtml (header man) : map toHtml (sections man)

-- The header can be converted to html
instance HTML Header where
   toHtml head = concatHtml $ map toHtml
      [htitle $ mtitle head
      , hinfo $ copy_notice $ copyright head
      , hinfo $ license_notice $ license head
      , hinfo $ license_file_notice $ license_file head] ++ map toHtml (preamble head)

htitle :: String -> Html
htitle = h1 . stringToHtml

hinfo :: String -> Html
hinfo = h2 . stringToHtml

section_title :: [Int] -> String -> Html
section_title nums section = 
   h2 $ (anchor $ stringToHtml sname) ! [name sname]
   where
   sname = section_name nums section

section_name :: [Int] -> String -> String
section_name nums section = pretty_nums nums "" ++ section

-- A section can be converted to html
instance HTML Section where
   toHtml sec = concatHtml $
      section_title (number sec) (title sec) : map toHtml (stext sec) ++ map toHtml (subsections sec)

instance HTML Paragraph where
   toHtml para =
      paragraph $ concatHtml $ ptext para

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
