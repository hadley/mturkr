context("Templates")

template_path <- system.file("template", package = "mturkr")

test_that("one template for each row of data", {
  plates <- render_templates(template_path)
  expect_equal(length(plates), 3)
})

test_that("rendered templates are valid xml", {
  plates <- render_templates(template_path)
  plates_xml <- lapply(plates, xmlTreeParse, asText = TRUE)
  
  validation <- lapply(plates, validate_question_xml)
  success <- vapply(validation, as.logical, logical(1))
  
  expect_true(all(success))
})