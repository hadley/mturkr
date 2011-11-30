#' Get all assignments for a task.
#'
#' Most of the time you should not need to call this directly - instead use
#' \code{\link{review_task}}.
#'
#' @inheritParams publish_task
#' @param status Which status of assignments to retrieved: "approved",
#'   "rejected", or "submitted" (i.e. assignments waiting approval).
#' @return A data frame with a column for each question (named according to
#'   the question identifier. Multiple anwsers (e.g. from multiple selection
#'   questions) are separated with the unicode record separater, 
#'   \code{"\u{001F}"}. Additional columns provided by MTurk data are
#'   assigmnent_id, worker_id, auto_approval_time, accept_time and
#'   submit_time.
#' @export
#' @importFrom plyr ldply
get_assignments <- function(task = NULL, status, ...) {
  task <- as.task(task)
  template <- load_template(task)
  
  ids <- template$hit_id[!is.na(template$hit_id)]

  ldply(ids, get_assignments_for_hit, task = task, status = status, ...)
}

#' @importFrom plyr ldply
get_assignments_for_hit <- function(hit_id, task, status, ...) {
  status <- match.arg(status, c("approved", "rejected", "submitted"))
  
  # Find out how many pages of results there are
  xml <- mturk_task_req(task, "GetAssignmentsForHIT", 
    HITId = hit_id, PageSize = 1, PageNumber = 1, ...)
  total <- as.numeric(xmlValue(
    getNodeSet(xml, "//TotalNumResults")[[1]][[1]]))
  pages <- ceiling(total / 100)

  # Load all results into a single data frame
  get_ids <- function(page) {
    xml <- mturk_task_req(task, "GetAssignmentsForHIT", 
      HITId = hit_id, PageSize = 100, PageNumber = page, ...)
    ldply(getNodeSet(xml, "//Assignment"), extract_answers)
  }
  
  ldply(seq_len(pages), get_ids)
}

#' @importFrom plyr ldply
extract_answers <- function(assignment) {
  assignment_id <- xmlValue(assignment[["AssignmentId"]])
  worker_id <- xmlValue(assignment[["WorkerId"]])
  status <- xmlValue(assignment[["AssignmentStatus"]])
  auto_approval_time <- parse_time(xmlValue(assignment[["AutoApprovalTime"]]))
  accept_time <- parse_time(xmlValue(assignment[["AcceptTime"]]))
  submit_time <- parse_time(xmlValue(assignment[["SubmitTime"]]))
  
  answer_raw <- xmlValue(assignment[["Answer"]])
  answer_xml <- xmlTreeParse(answer_raw)$doc$children[[1]]
  
  ns <- c(qa = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionFormAnswers.xsd")
  answers <- getNodeSet(answer_xml, "//qa:Answer", ns)
  
  question_ids <- vapply(answers, function(x) xmlValue(x[["QuestionIdentifier"]]), character(1))
  
  collapse_answers <- function(xml) {
    answer_nodes <- "//FreeText|//SelectionIdentifier|//OtherSelectionText"
    a_nodes <- getNodeSet(xml, answer_nodes)
    str_c(vapply(a_nodes, xmlValue, character(1)), collapse = "\u{001F}")
  }
  answers <- lapply(answers, collapse_answers)
  names(answers) <- question_ids
  # Can't remember how to turn a list into a data frame :(
  answers_df <- structure(answers, class = "data.frame", row.names = 1L)

  data.frame(assignment_id, worker_id, status, auto_approval_time,
    accept_time, submit_time, answers_df)
}

