module Cases where

import Test.Tasty.HUnit ((@?=), testCase)
import Test.Tasty (TestTree, testGroup)
import Data.HaskID (encode, haskid, init_haskid, opts, HashOptions(..))

-- test cases from github.com/hashids-python
encode_cases :: TestTree
encode_cases = testGroup "encoding test cases" [
      testCase "empty_call" $ encode haskid [] @?= ""

    , testCase "default_salt" $ encode haskid [1, 2, 3] @?= "o2fXhV"

    , testGroup "single_number" $ let h = haskid
    in zipWith (testCase . show) [0..] [
          encode h [12345] @?= "j0gW"
        , encode h [1] @?= "jR"
        , encode h [22] @?= "Lw"
        , encode h [333] @?= "Z0E"
        , encode h [9999] @?= "w0rR"
        ]

    , testGroup "multiple_numbers" $ let h = haskid
    in zipWith (testCase . show) [0..] [
          encode h [683, 94108, 123, 5] @?= "vJvi7On9cXGtD"
        , encode h [1, 2, 3] @?= "o2fXhV"
        , encode h [2, 4, 6] @?= "xGhmsW"
        , encode h [99, 25] @?= "3lKfD"
        ]

    , testGroup "salt" $ let
        Right h = init_haskid opts { opt_salt = "Arbitrary string" }
    in zipWith (testCase . show) [0..] [
          encode h [683, 94108, 123, 5] @?= "QWyf8yboH7KT2"
        , encode h [1, 2, 3] @?= "neHrCa"
        , encode h [2, 4, 6] @?= "LRCgf2"
        , encode h [99, 25] @?= "JOMh1"
        ]

    , testGroup "alphabet" $ let
        Right h = init_haskid opts { opt_alphabet =
            "!\"#%&\',-/0123456789:;<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ_`abcdefghijkl\
            \mnopqrstuvwxyz~" }
    in zipWith (testCase . show) [0..] [
          encode h [2839, 12, 32, 5] @?= "_nJUNTVU3"
        , encode h [1, 2, 3] @?= "7xfYh2"
        , encode h [23832] @?= "Z6R>"
        , encode h [99, 25] @?= "AYyIB"
        ]

    , testGroup "min_length" $ let
        Right h = init_haskid opts { opt_min_length = 25 }
    in zipWith (testCase . show) [0..] [
          encode h [7452, 2967, 21401] @?= "pO3K69b86jzc6krI416enr2B5"
        , encode h [1, 2, 3] @?= "gyOwl4B97bo2fXhVaDR0Znjrq"
        , encode h [6097] @?= "Nz7x3VXyMYerRmWeOBQn6LlRG"
        , encode h [99, 25] @?= "k91nqP3RBe3lKfDaLJrvy8XjV"
        ]

    , testGroup "all_parameters" $ let
        Right h = init_haskid opts
            { opt_salt = "arbitrary salt"
            , opt_alphabet = "abcdefghijklmnopqrstuvwxyz"
            , opt_min_length = 16
            }
    in zipWith (testCase . show) [0..] [
          encode h [7452, 2967, 21401] @?= "wygqxeunkatjgkrw"
        , encode h [1, 2, 3] @?= "pnovxlaxuriowydb"
        , encode h [60125] @?= "jkbgxljrjxmlaonp"
        , encode h [99, 25] @?= "erdjpwrgouoxlvbx"
        ]

    , testGroup "alphabet_without_standard_separators" $ let
        alpha' = "abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890"
        Right h = init_haskid opts { opt_alphabet = alpha', opt_min_length = 0 }
    in zipWith (testCase . show) [0..] [
          encode h [7452, 2967, 21401] @?= "X50Yg6VPoAO4"
        , encode h [1, 2, 3] @?= "GAbDdR"
        , encode h [60125] @?= "5NMPD"
        , encode h [99, 25] @?= "yGya5"
        ]

    , testGroup "alphabet_with_two_standard_separators" $ let
        alpha' = "abdegjklmnopqrvwxyzABDEGJKLMNOPQRVWXYZ1234567890uC"
        Right h = init_haskid opts { opt_alphabet = alpha' }
    in zipWith (testCase . show) [0..] [
          encode h [7452, 2967, 21401] @?= "GJNNmKYzbPBw"
        , encode h [1, 2, 3] @?= "DQCXa4"
        , encode h [60125] @?= "38V1D"
        , encode h [99, 25] @?= "373az"
        ]

    , testCase "negative_call" $ encode haskid [1, -2, 3] @?= ""
    ]

def_alpha :: String
def_alpha = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
