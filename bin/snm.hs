-- Generate simple and nice looking manuals

import Manual

import System.Environment
import System.Console.GetOpt

import Data.Maybe

import Data.List

import System.FilePath

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe

import System.IO

-- Flags determine how we should behave
data ManFlag =
     Dump
   | OutputFile (Maybe String)
   | OutputType {out_type :: OutputType}
   | Help
   deriving Eq


-- Match only output types
only_types :: ManFlag -> Bool
only_types (OutputType _) = True
only_types _ = False

-- Match only output flag
only_output :: ManFlag -> Bool
only_output (OutputFile _) = True
only_output _ = False

-- Get the output file
output_dir :: FilePath -> ManFlag -> FilePath
output_dir defaul flg =
   case flg of
      OutputFile ms -> fromMaybe defaul ms
      _             -> error "Bug in snm.hs::output_dir!\nContact John Morrice at spoon@killersmurf.com"

-- The order of the arguments
order :: ArgOrder ManFlag 
order = Permute

-- The options we use
options :: [OptDescr ManFlag]
options =
   [dump, output_file, help, text_out, xhtml_out]

-- The user is requesting help
help :: OptDescr ManFlag
help =
   Option "h" ["help"] (NoArg Help) "Print usage information"

-- The user requests output in xhtml
xhtml_out :: OptDescr ManFlag
xhtml_out =
   Option "xh" ["xhtml", "html"] (NoArg $ OutputType XHtml) "Output in xhtml format." 

-- The user requests output in text
text_out :: OptDescr ManFlag
text_out =
   Option "t" ["text", "txt"] (NoArg $ OutputType Text) "Text output."

-- The user requests ouput dumped to stdout
dump :: OptDescr ManFlag
dump = 
   Option "d" ["dump"] (NoArg Dump) "Print output to stdout."

-- The user requests output dumped to file
output_file :: OptDescr ManFlag
output_file =
   Option "o" ["output"] (OptArg OutputFile "A file name") "Write output to a file.  Multiple outputs ignored, printing warning to stderr."

-- Print usage information
usage :: IO ()
usage = do
   nm <- getProgName
   putStrLn $ usageInfo (nm ++ ": The Simple Nice-Looking Manual Generator") options


-- "Make it so" - Jean Luc Picard. 
main :: IO ()
main = do
   -- get the args
   as <- getArgs
   -- get the options
   let (unclean_flags, non, err) = getOpt order options as
       -- clean the flags
       flags = nub unclean_flags
       user_outputs = catMaybes $ map (flip M.lookup formats) $ map out_type $ filter only_types flags

       outputs = if null user_outputs then [xhtml_format] else user_outputs
   -- terminate if there were errors
   if null err
      then do
         case non of
            (input:[]) -> do
               man <- load_manual input 
               -- did the user tell us what to do?
               if null flags
                  then
                     mapM_ (uncurry (undirected input man)) outputs
                  else
                     if elem Help flags
                        then usage
                        else 
                           mapM_ (uncurry (directed man flags input)) outputs
            _ -> usage
      else 
         usage
      where
     
      -- The user has specified what to do
      directed man flags defaul format ext = do
         let outs = filter only_output flags
             dump = elem Dump flags 
         -- dump the text if dump is present
         when dump $ putStrLn $ format man
         nm <- getProgName
         mout <- case outs of
                    (out:[]) -> return $ Just out
                    (out:n:_) -> do
                       hPutStrLn stderr 
                                 ("Warning: " ++ nm ++ " multiple outputs ignored.")
                       return (Just out)
                    _        -> return Nothing
         maybe (return ()) 
               (\out ->
                  write_manual format ext man $ output_dir defaul out)
               mout

      -- The default behaviour
      undirected input man format ext =
         write_manual format ext man (dropExtensions $ takeFileName input)


