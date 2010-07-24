-- Generate simple and nice looking manuals

import Manual

import System.Environment
import System.Console.GetOpt

import Data.Maybe

import Data.List

import System.FilePath

import Control.Monad

-- Flags determine how we should behave
data ManFlag =
     Dump
   | OutputFile (Maybe String)
   | Help
   deriving Eq

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
   [dump, output_file, help]

-- The user is requesting help
help :: OptDescr ManFlag
help =
   Option "h" ["help"] (NoArg Help) "Print usage information"

-- The user requests ouput dumped to stdout
dump :: OptDescr ManFlag
dump = 
   Option "d" ["dump"] (NoArg Dump) "Print output to stdout."

-- The user requests output dumped to file
output_file :: OptDescr ManFlag
output_file =
   Option "o" ["output"] (OptArg OutputFile "A file name") "Write output to a file."

-- Print usage information
usage :: IO ()
usage = do
   nm <- getProgName
   putStrLn $ usageInfo (nm ++ " input [('-o'|'--output') [output] | ('-d'|'--dump') | ('-h'|'--help')]") options

-- "Make it so" - Jean Luc Picard. 
main :: IO ()
main = do
   -- get the args
   as <- getArgs
   -- get the options
   let (unclean_flags, non, err) = getOpt order options as
       -- clean the flags
       flags = nub unclean_flags
   -- terminate if there were errors
   if null err
      then do
         case non of
            (input:[]) -> 
               -- did the user tell us what to do?
               if null flags
                  then
                     undirected input
                  else
                     if elem Help flags
                        then usage
                        else directed input flags
            _ -> usage
      else 
         usage
      where
     
      -- The user has specified what to do
      directed input flags = do
         txt <- create_text_manual input
         let outs = map (output_dir input) $ filter only_output flags
             dump = elem Dump flags 
         -- dump the text if dump is present
         when dump $ putStrLn txt
         mapM_ (flip write_txt_file txt) outs

      -- The default behaviour
      undirected input = do
         txt <- create_text_manual input
         write_txt_file txt (dropExtensions $ takeFileName input)


