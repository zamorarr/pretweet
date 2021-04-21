test_that("re_mention works", {
  x <- c("hey @bob and @alice", "ok whatever @charlie", "haha")

  #actual <- stringr::str_match_all(x, re_mention)
  #actual <- lapply(actual, function(a) a[,1])
  actual <- regmatches(x, gregexpr(re_mention, x, perl = TRUE))
  expected <- list(c("@bob", "@alice"), "@charlie", character(0))

  expect_identical(actual, expected)
})

test_that("extract patterns for emojis", {
  x <- "★★★★"

  actual <- regmatches(x, gregexpr(re_twitter, x, perl = TRUE))[[1]]
  expected <- rep(substr(x, 1, 1), 4)

  expect_equal(actual, expected)
})
