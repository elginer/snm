import Test.Simple

import Manual.Reader
import Manual.Structure

main = test_all_IO "load_section" load_test

load_test :: FilePath -> String -> IO (Either String Section)
load_test fp src =
   fmap Right $ load_section fp
