
#' @export
render_templates <- function(task, data = load_template(task)) {
  task <- as.task(task)
  
  
  questions_path <- file.path(task$path, "questions.xml")
  if (!file.exists(questions_path)) {
    stop(questions_path, " doesn't exist", call. = FALSE)
  }
  
  template <- str_c(readLines(questions_path), collapse = "")
  
  lapply(seq_len(nrow(data)), function(i) {
    render_template(template, as.list(data[i, , drop = FALSE]))
  })
}

load_template <- function(task) {
  data_path <- file.path(task$path, "template.csv")
  if (!file.exists(data_path)) {
    stop(data_path, " doesn't exist", call. = FALSE)
  }
  read.csv(data_path, stringsAsFactors = FALSE)
}

#' @importFrom whisker whisker.render
#' @importFrom stringr str_c
render_template <- function(template, data) {
  stopifnot(is.list(data))
  stopifnot(length(names(data)) == length(data))
  
  whisker.render(template, data)
}
