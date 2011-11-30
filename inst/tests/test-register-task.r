context("Register task")

test_that("Reward parsed as expected", {
  res <- parse_reward("0.01")
  expect_equal(res$amt, 0.01)
  expect_equal(res$cur, "USD")
  
  res <- parse_reward("1 CAD")
  expect_equal(res$amt, 1)
  expect_equal(res$cur, "CAD")
})

test_that("time limit parsed correctly",  {
  expect_equal(parse_duration("1"), 1)
  expect_equal(parse_duration("1 s"), 1)
  expect_equal(parse_duration("1 sec"), 1)
  expect_equal(parse_duration("1 second"), 1)
  expect_equal(parse_duration("1 seconds"), 1)

  expect_equal(parse_duration("1 second"), 1)
  expect_equal(parse_duration("1 minute"), 60)
  expect_equal(parse_duration("1 hour"), 3600)
  expect_equal(parse_duration("1 day"), 86400)
  expect_equal(parse_duration("1 week"), 86400 * 7)
  
  expect_equal(parse_duration("5 minutes"), 5 * 60)
  expect_equal(parse_duration("10 minutes"), 10 * 60)
})