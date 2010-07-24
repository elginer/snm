import Test.Simple

import Manual.Reader
import Manual.Structure

import Data.Yaml.Simple

main =
   test_all_IO "paragraph" paragraph_test

paragraph_test :: FilePath -> String -> IO (Either String Paragraph)
paragraph_test fp src = do
   y <- parse_yaml src
   let p = from_yaml y
   putStrLn $ "Paragraph: " ++ show p 
   return $ Right p
