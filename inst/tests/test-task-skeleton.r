context("Task skeleton")

template_path <- system.file("template", package = "mturkr")

test_that("task_skeleton can't override existing file/directory", {
  expect_that(task_skeleton("."), throws_error("already exists"))
})

test_that("task_skeleton copies all files in template", {
  should_copy <- dir(template_path, recursive = TRUE)
  
  dest <- tempfile()
  
  task_skeleton(dest)
  expect_equal(dir(dest, recursive = TRUE), should_copy)
  
  unlink(dest)
})

test_that("xml files are valid", {
  expect_true(as.logical(validate_questions(template_path)))
})