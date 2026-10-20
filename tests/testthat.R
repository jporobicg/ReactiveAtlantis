library(testthat)

test_dir <- if (dir.exists("tests/testthat")) "tests/testthat" else "testthat"
testthat::test_dir(test_dir)
