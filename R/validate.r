schema_base_url <- str_c("http://mechanicalturk.amazonaws.com/",
  "AWSMechanicalTurkDataSchemas/")

#' @importFrom stringr str_c
#' @export
validate_questions <- function(task) {
  task <- as.task(task)
  questions_path <- file.path(task$path, "questions.xml")
  
  if (!file.exists(questions_path)) {
    stop(questions_path, " does not exist", call. = FALSE)
  }
  
  schema_url <- str_c(schema_base_url, "2005-10-01/QuestionForm.xsd")
  schema <- xmlSchemaParse(schema_url)
  
  xmlSchemaValidate(schema, questions_path)
}


#' @importFrom stringr str_c str_replace_all
#' @S3method format XMLError
format.XMLError <- function(x, ...) {
  x$msg <- str_replace_all(str_trim(x$msg), "\\{.*?\\}", "")
  str_c(basename(x$filename), ":",  x$line, ":", x$col, " ", x$msg)
}

#' @S3method print XMLError
print.XMLError  <- function(x, ...) print(format(x, ...))

#' @S3method format XMLSchemaValidationResults
format.XMLSchemaValidationResults <- function(x, ...) {
  if (as.logical(x)) return("Success")
  
  errors <- vapply(x$errors, format, character(1))
  warnings <- vapply(x$warnings, format, character(1))
  
  wrap <- function(x) str_wrap(str_c(errors, collapse = "\n\n"), exdent = 2)
  
  str_c(
    "Failure\n",
    if (length(errors) > 0) str_c("Errors:\n", wrap(errors)),
    if (length(warnings) > 0) str_c("Errors:\n", wrap(warnings))
  )
}

#' @S3method format XMLSchemaValidationResults
print.XMLSchemaValidationResults <- function(x, ...) {
  cat(format(x, ...), "\n")
}

as.logical.XMLSchemaValidationResults <- function(x, ...) {
  x$status == 0
}
