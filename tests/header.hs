import Test.Simple

import Manual.Reader
import Manual.Structure

import Data.Yaml.Simple

main =
   test_all_IO "header" header_test

header_test :: FilePath -> String -> IO (Either String Header)
header_test fp src = do
   y <- parse_yaml src
   let p = from_yaml y
   putStrLn $ "Header: " ++ show p 
   return $ Right p
