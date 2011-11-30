context("Register task")

test_that("Reward parsed as expected", {
  res <- parse_reward("0.01")
  expect_equal(res[[1]], "0.01")
  expect_equal(res[[2]], "USD")
  
  res <- parse_reward("1 CAD")
  expect_equal(res[[1]], "1")
  expect_equal(res[[2]], "CAD")
})