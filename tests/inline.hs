-- Test parsing of inline paragraph elements works

import Manual.Reader
import Test.Simple

main = test_all "inline" inline_test

inline_test fp =
   eparse_inline
