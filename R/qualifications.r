qual_env <- local({
  "==" <- function(e1, e2) new_qr(substitute(e1), "EqualTo", as.integer(e2))
  "!=" <- function(e1, e2) new_qr(substitute(e1), "NotEqualTo", as.integer(e2))
  "<" <- function(e1, e2)  new_qr(substitute(e1), "LessThan", as.integer(e2))
  ">" <- function(e1, e2)  new_qr(substitute(e1), "GreaterThan", as.integer(e2))
  "<=" <- function(e1, e2) new_qr(substitute(e1), "LessThanOrEqualTo", as.integer(e2))
  ">=" <- function(e1, e2) new_qr(substitute(e1), "GreaterThanOrEqualTo", as.integer(e2))
  "exists" <- function(x) new_qr(x, "Exists")
  "in_locale" <- function(locale) {}
    
  "need_for_preview" <- function(x) {
    stopifnot(is.qr(x))
    x$need_for_preview <- TRUE
    x
  }
  
  environment()
})

new_qr <- function(id, comparator, value = NULL) {
  structure(list(id = id, comparator = comparator, value = value, 
    need_for_preview = FALSE), class = "qr")
}

is.qr <- function(x) inherits(x, "qr")

format.qr <- function(x, ...) {
  paste("<QualificationRequirement>\n", 
    "  <QualificationTypeId>", x$id, "</QualificationTypeId>\n", 
    "  <Comparator>", x$comparator, "</Comparator>\n", 
    "  <IntegerValue>", x$value, "</IntegerValue>\n",
    if (x$need_for_preview) "  <RequiredToPreview>TRUE</RequiredToPreview>\n",
    "</QualificationRequirement>\n", sep = "")
}
print.qr <- function(x, ...) {
  cat(format(x), "\n")
}

as_qr <- function(code) {
  expr <- substitute(code)
  if (length(expr) == 3 && identical(expr[[1]], as.name("{"))) {
    exprs <- as.list(expr[-1])
  } else {
    exprs <- list(expr)
  }
  lapply(exprs, eval, qual_env)
}
# 
# as_qr("hadley" == 4)
# 
# as_qr({
#   hadley == 4
#   need_for_preview(john == 5)
# })
