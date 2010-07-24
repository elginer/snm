import Test.Simple

import Manual.Reader
import Manual.Structure

import Data.Yaml.Simple

main =
   test_all_IO "section" section_test

section_test :: FilePath -> String -> IO (Either String Section)
section_test fp src = do
   y <- parse_yaml src
   let p = from_yaml y
   putStrLn $ "Section: " ++ show p 
   return $ Right p
