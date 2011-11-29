context("Task skeleton")

test_that("task_skeleton can't override existing file/directory", {
  expect_that(task_skeleton("."), throws_error("already exists"))
})

test_that("task_skeleton copies all files in template", {
  should_copy <- dir(system.file("task-demo", package = "mturkr"), 
    recursive = TRUE)
  
  dest <- tempfile()
  
  task_skeleton(dest)
  expect_equal(dir(dest, recursive = TRUE), should_copy)
  
  unlink(dest)
})
