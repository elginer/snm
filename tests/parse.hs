import Test.Simple

import Data.Yaml.Simple

main = test_all_IO "parser" parse_test

parse_test :: FilePath -> String -> IO (Either String Yaml)
parse_test fp src =
   fmap Right $ parseYaml src
