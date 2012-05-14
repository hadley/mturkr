#' Review task.
#'
#' Reviewing takes place in two steps: reviewable HITs are downloaded and
#' stored in \code{hit-review.csv}, and then the review column is inspected
#' and matched to approve or reject (or prefixes) and those reviews are 
#' uploaded back to MTurk and removed from the csv file.
#'
#' @export
#' @inheritParams publish_task
#' @importFrom plyr rbind.fill
review_task <- function(task = NULL, ..., quiet = FALSE) {
  task <- as.task(task)
  type_id <- task$HitTypeId[1]
  
  # Load existing reviews and add in any new ones that we haven't seen before
  reviews <- load_task_csv(task, "hit-reviews")  
  assignments <- get_assignments(task, "submitted", ...)
  new_reviews <- !(assignments$assignment_id %in% reviews$assignment_id)
  
  n_new <- sum(new_reviews)
  if (n_new > 0) {
    if (!quiet) message(n_new, " new assignments to review")
    
    reviews <- rbind.fill(reviews, assignments[new_reviews, , drop = FALSE])
  }
  
  if (is.null(reviews$status)) reviews$status <- rep("", nrow(reviews))
  if (is.null(reviews$feedback)) reviews$feedback <- rep("", nrow(reviews))
  
  # Approve/reject reviewed assignments
  to_review <- which(!is.na(reviews$status) & reviews$status != "")
  n_reviews <- length(to_review)
  
  for(i in seq_along(to_review)) {
    rev <- as.list(reviews[to_review[i], , drop = FALSE])
    if (!quiet) {
      message("Reviewing assignment [", i, "/", n_reviews, "] ",
        rev$assignment_id)      
    }
    
    try(review_assignment(task, rev$assignment_id, rev$status, rev$feedback))
  }
  
  if (n_reviews > 0) {
    # rev <- reviews[to_review, , drop = FALSE]
    # review_assignments(task, rev$assignment_id, rev$status, rev$feedback)
    
    old_results <- load_task_csv(task, "hit-results")
    new_results <- reviews[to_review, , drop = FALSE]
    results <- rbind.fill(old_results, new_results)
    
    save_task_csv(task, "hit-results", results)
  }
  
  reviews <- reviews[is.na(reviews$status) | reviews$status == "", , 
    drop = FALSE]
  save_task_csv(task, "hit-reviews", reviews)
}


load_task_csv <- function(task, name) {
  task <- as.task(task)
  path <- file.path(task$path, str_c(name, ".csv"))
  
  if (file.exists(path)) {
    read.csv(path, stringsAsFactors = FALSE)
  } else {
    data.frame()
  }
}

save_task_csv <- function(task, name, df) {
  if (is.null(df) || nrow(df) == 0) return()
  
  task <- as.task(task)
  path <- file.path(task$path, str_c(name, ".csv"))
  write.csv(df, path, row.names = FALSE)
}
