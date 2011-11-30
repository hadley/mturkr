#' Review task.
#'
#' Reviewing takes place in two steps: reviewable HITs are downloaded and
#' stored in \code{hit-review.csv}, and then the review column is inspected
#' and matched to approve or reject (or prefixes) and those reviews are 
#' uploaded back to MTurk and removed from the csv file.
#'
#' @export
#' @inheritParams publish_task
review_task <- function(task = NULL, ...) {
  task <- as.task(task)
  type_id <- task$HitTypeId[1]
  
  get_page <- function(page = 1) {
    mturk_task_req(task, "SearchHITs", 
      HitTypeID = type_id,
      SortProperty = "Enumeration",
      PageSize = 100,
      PageNumber = page,
      ...)
  }
  
  reviews <- load_reviews()
  get_page(1)
  
  
  
}




load_reviews <- function(task = NULL) {
  task <- as.task(task)
  
  reviews_path <- file.path(task$path, "hit-review.csv")
  
  if (file.exist(reviews_path)) {
    read.csv(reviews_path, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
}
