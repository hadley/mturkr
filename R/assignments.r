#' Get all assignments for a task.
#'
#' Most of the time you should not need to call this directly - instead use
#' \code{\link{review_task}}.
#'
#' @inheritParams publish_task
#' @param status Which assignment status to retrieve: "approved",
#'   "rejected", or "submitted" (i.e. assignments awaiting approval).
#' @return A data frame with a column for each question (named according to
#'   the question identifier. Multiple anwsers (e.g. from multiple selection
#'   questions) are separated with the unicode record separater, 
#'   \code{"\u{001F}"}. Additional columns provided by MTurk data are
#'   assigmnent_id, worker_id, auto_approval_time, accept_time and
#'   submit_time.
#' @export
#' @importFrom plyr ldply
#' @importFrom stringr str_sub str_sub<-
get_assignments <- function(task = NULL, status, ...) {
  task <- as.task(task)
  template <- load_template(task)
  
  ids <- template$hit_id[!is.na(template$hit_id)]

  status <- match.arg(status, c("approved", "rejected", "submitted"))
  str_sub(status, 1, 1) <- toupper(str_sub(status, 1, 1))
  
  ldply(ids, get_assignments_for_hit, task = task, status = status,
    ...)
}

#' @importFrom plyr ldply
#' @importFrom XML xmlValue getNodeSet
get_assignments_for_hit <- function(hit_id, task, status, ...) {  
  # Find out how many pages of results there are
  xml <- mturk_task_req(task, "GetAssignmentsForHIT", 
    AssignmentStatus = status, HITId = hit_id, 
    PageSize = 1, PageNumber = 1, ...)
  total <- as.numeric(xmlValue(
    getNodeSet(xml, "//TotalNumResults")[[1]][[1]]))
  pages <- ceiling(total / 100)  

  # Load all results into a single data frame
  get_ids <- function(page) {
    xml <- mturk_task_req(task, "GetAssignmentsForHIT",  
      AssignmentStatus = status, HITId = hit_id, 
      PageSize = 100, PageNumber = page, ...)
    ldply(getNodeSet(xml, "//Assignment"), extract_answers)
  }
  
  ldply(seq_len(pages), get_ids)
}

#' @importFrom plyr ldply
extract_answers <- function(assignment) {
  assignment_id <- xmlValue(assignment[["AssignmentId"]])
  worker_id <- xmlValue(assignment[["WorkerId"]])
  assignment_status <- xmlValue(assignment[["AssignmentStatus"]])
  auto_approval_time <- parse_time(xmlValue(assignment[["AutoApprovalTime"]]))
  accept_time <- parse_time(xmlValue(assignment[["AcceptTime"]]))
  submit_time <- parse_time(xmlValue(assignment[["SubmitTime"]]))
  
  ns <- c(qa = "http://mechanicalturk.amazonaws.com/AWSMechanicalTurkDataSchemas/2005-10-01/QuestionFormAnswers.xsd")

  answer_raw <- xmlValue(assignment[["Answer"]])
  answer_xml <- xmlTreeParse(answer_raw)$doc$children[[1]]
  
  answers <- getNodeSet(answer_xml, "//qa:Answer", ns)  
  question_ids <- vapply(answers, FUN.VALUE = character(1),
    function(x) xmlValue(x[["QuestionIdentifier"]]))
  
  collapse_answers <- function(xml) {
    answer_nodes <- "//FreeText|//SelectionIdentifier|//OtherSelectionText"
    a_nodes <- getNodeSet(xml, answer_nodes)
    str_c(vapply(a_nodes, xmlValue, character(1)), collapse = "\u{001F}")
  }
  answers <- lapply(answers, collapse_answers)
  names(answers) <- question_ids

  answers_df <- as.data.frame(answers)

  data.frame(assignment_id, worker_id, auto_approval_time, assignment_status,
    accept_time, submit_time, answers_df, stringsAsFactors = FALSE)
}


review_assignment <- function(task, assignment_id, status, feedback = NULL) {
  status <- match.arg(tolower(status), c("approve", "reject"))
  ops <- c("approve" = "ApproveAssignment", "reject" = "RejectAssignment")
  
  mturk_task_req(task, ops[[status]], 
    AssignmentId = assignment_id,
    RequesterFeedback = feedback)
  TRUE
}
