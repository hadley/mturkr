#' Register a task (HITType).
#' 
#' Register your task as a HITType on MTurk. The HIT id is stored in your
#' task DESCRIPTION file so that there's always a record of which HIT types
#' have been generated from this task - there may be multiple if you have
#' changed the title, description, keywords etc.
#'
#' If this task has been registered previously, and you have changed any of 
#' the HIT type metadata, you'll get a warning notification.
#'
#' @section Required metadata:
#'
#' \itemize{
#'   \item \code{Reward}: Dollars to pay for reward, e.g. \code{0.05} for 
#'     5 cents.  Can optionally supply currency code for non-USD amounts, e.g.
#'     \code{0.05 CAD}.
#'   
#'   \item \code{TimeLimit}: Maximum amount of time a worker is allowed to 
#'     take to complete each HIT. Can be an integer (\code{1}), or can specify
#'     seconds, minutes, hours, days or weeks (or any unique prefix):
#'     \code{40 seconds}, \code{5 minutes}, \code{1 day}, \code{2 w}.
#' }
#' @param The location of an mturk task, see \code{\link{as.task}} for
#'   specification options
#' @param ... Other parameters passed on to \code{\link{mturk_task_req}}.
#' @return the HIT type ID
#' @export
register_task <- function(task = NULL, ...) {
  task <- as.task(task)
  
  # qual <- parse_qualification(task$Qualifications)
  duration <- parse_duration(task$TimeLimit)
  reward <- parse_reward(task$Reward)

  result <- mturk_task_req(task, "RegisterHITType", 
    Title = task$Title,
    Description = task$Description,
    Keywords = task$Keywords,
    # QualificationRequirement = qual,
    AssignmentDurationInSeconds = duration,
    Reward.1.Amount = reward$amt,
    Reward.1.CurrencyCode = reward$cur,
    ...)
    
  id <- xmlValue(result[["HITTypeId"]][[1]])
  existing <- task$HitTypeId
  if (!(id %in% existing)) {
    if (length(existing) > 0) {
      warning("Creating new HIT type id: ", id, call. = FALSE)
    }
    task$HitTypeId <- c(id, existing)
  }
  
  save_task(task)
  
  id
}

parse_reward <- function(x) {
  pieces <- str_split(x, " ")[[1]]
  if (length(pieces) == 1) {
    amt <- as.numeric(pieces)
    cur <- "USD"
  } else {
    amt <- as.numeric(pieces[1])
    cur <- pieces[2]
  }
  
  list(amt = amt, cur = cur)
}

parse_duration <- function(x) {
  pieces <- str_split(x, " ")[[1]]
  if (length(pieces) == 1) return(as.numeric(pieces))
  
  seconds <- cumprod(c(seconds = 1, minutes = 60, hours = 60, 
    days = 24, weeks = 7))
  
  unit <- match.arg(pieces[2], names(seconds))
  as.numeric(pieces[1]) * seconds[[unit]]
}
