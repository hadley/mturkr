#' @importFrom stringr str_c
mturk_req_url <- function(host = "sandbox", ...) {
  host <- match.arg(host, c("sandbox", "production"))
  host2 <- c(
    "sandbox" = "mechanicalturk.sandbox.amazonaws.com",
    "production" = "mechanicalturk.amazonaws.com"
  )[host]
  
  base <- str_c("https://", host2)
  
  params <- c(...,
    Service = "AWSMechanicalTurkRequester",
    Version = "2008-08-02"
  )
  params <- vapply(params, URLencode, reserved = TRUE, 
    FUN.VALUE = character(1))
  
  str_c("https://", host2, "?", 
    str_c(names(params), "=", params, collapse = "&"))
}

#' @importFrom stringr str_wrap
mturk_req <- function(host = "sandbox", ...) {
  url <- mturk_req_url(host, ...)
  message(url)
  
  result <- getURL(url)
  xml <- xmlTreeParse(result)$doc$children[[1]]
  
  request <- xml[["OperationRequest"]]
  
  if (!is.null(request[["Errors"]])) {
    err <- xml[["Errors"]][[1]]
    code <- toString(err[["Code"]][[1]])
    msg <- toString(err[["Message"]][[1]])
    
    stop(str_wrap(str_c("[", code, "] ", msg), exdent = 2), call. = FALSE)
  }
  
  xml
}

#' Perform operation for given task.
#' 
#' @section Keys:
#' To use amazon mturk you need both an access key (analogous to a username)
#' and a secret key (analogous to a password). Both are available from \url{https://aws-portal.amazon.com/gp/aws/developer/account/index.html?action=access-key#access_credentials}.
#'
#' There are three ways to specify your keys:
#'
#' \itemize{
#'   \item As parameters to any request function: \code{access_key},
#'     \code{secret_key}
#'   \item As parameters in your task description: \code{AccessKey},
#'     \code{SecretKey}
#'   \item As environmental variables: \code{AWS_KEY}, \code{AWS_SECRET_KEY}
#' }
#'
#' If your code is publically available (such as through a public svn or 
#' git repository) DO NOT store your secret in your task description, as 
#' this will allow anyway to charge jobs to your amazon account. If you do
#' accidentally publish it, go to the url above and deactivate it and 
#' generate new access and secret keys.
mturk_task_req <- function(task, operation, host = NULL, access_key = NULL, secret_key = NULL, ...) {
  task <- as.task(task)
  
  host <- host %||% task$Host
  access_key <- access_key %||% task$AccessKey %||% Sys.getenv("AWS_KEY")

  secret_key <- secret_key %||% task$SecretKey %||%
   Sys.getenv("AWS_SECRET_KEY")
  time <- timestamp()
  sig <- make_signature(secret_key, operation, time)
  
  mturk_req(host = host, AWSAccessKeyId = access_key, Operation = operation, 
    Timestamp = time, Signature = sig, ...)
}


#' Make HMAC encoded signature for all AWS requests.
#' 
#' @importFrom digest hmac
#' @importFrom stringr str_c
#' @importFrom RCurl base64
#' @return base64 encoded signature
make_signature <- function(secret_key, operation, timestamp) {
  service <- "AWSMechanicalTurkRequester"
  
  sig_val <- str_c(service, operation, timestamp)
  hash <- hmac(secret_key, sig_val, "sha1", raw = TRUE)
  base64(hash)
}
