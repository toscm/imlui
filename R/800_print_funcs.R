# Cat Functions
caterr <- function(..., end = "\n") {
  cat(..., end, file = stderr())
}
cat2e <- function(...) {
  toscutil::cat2(..., file = stderr())
}
cat0e <- function(...) {
  toscutil::cat0(..., file = stderr())
}
catfe <- function(...) {
  toscutil::catf(..., file = stderr())
}
catne <- function(...) {
  toscutil::catn(..., file = stderr())
}
cat00e <- function(...) {
  toscutil::cat0(..., file = stderr())
}
cat0ne <- function(...) {
  toscutil::cat0n(..., file = stderr())
}
catfne <- function(...) {
  toscutil::catfn(..., file = stderr())
}
catnne <- function(...) {
  toscutil::catnn(..., file = stderr())
}
catsne <- function(...) {
  toscutil::catsn(..., file = stderr())
}
catsse <- function(..., sep = " ", end = " ") {
  toscutil::cat2(..., sep = sep, end = end, file = stderr())
}
cat0se <- function(..., sep = "", end = " ") {
  toscutil::cat2(..., sep = sep, end = end, file = stderr())
}

# Logging Functions
logerr <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    caterr(...)
  }
}
log2e <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat2e(...)
  }
}
log0e <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0e(...)
  }
}
logfe <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catfe(...)
  }
}
logne <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catne(...)
  }
}
log00e <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat00e(...)
  }
}
log0ne <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0ne(...)
  }
}
logfne <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catfne(...)
  }
}
lognne <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catnne(...)
  }
}
logsne <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catsne(...)
  }
}
logsse <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catsse(...)
  }
}
log0se <- function(...) {
  if(!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0se(...)
  }
}

# Helpers
now_ms <- function() {
  withr::with_options(
    new = list(digits.secs = 2), 
    code = as.character(lubridate::now())
  )
}
