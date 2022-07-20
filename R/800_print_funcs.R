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
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    caterr(...)
  }
}
log2e <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat2e(...)
  }
}
log0e <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0e(...)
  }
}
logfe <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catfe(...)
  }
}
logne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catne(...)
  }
}
log00e <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat00e(...)
  }
}
log0ne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0ne(...)
  }
}
logfne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catfne(...)
  }
}
lognne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catnne(...)
  }
}
logsne <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catsne(...)
  }
}
logsse <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    catsse(...)
  }
}
log0se <- function(...) {
  if (!isTRUE(getOption("imlui.suppress_log_messages"))) {
    cat0se(now_ms(), ":")
    cat0se(...)
  }
}

msg <- function(...,
                col = function(x) x,
                before = "",
                middle = glue("{now_ms()} {sid} {uid}"),
                after = ": ",
                sep = " ",
                end = "\n",
                cond = TRUE) {
  if (cond) {
    # If this function gets called from the server or any descendant, there
    # should be a `ses` object available in the parent (i.e. calling)
    # environments, containing the session ID (sid) and user ID (uid), which
    # we print. If there is no `ses` object available, we just print sid==0 and
    # uid=='null'. The only exeption is during `init_data`, i.e. when we create
    # the `ses` object. Here we directly look for the variable `sid`.
    ses <- dynGet("ses", NULL)
    if (is.null(ses)) {
      sid <- dynGet("sid", 0)
      uid <- "null"
    } else {
      sid <- ses$sid
      uid <- isolate(ses$rv$user$id) %||% "null"
    }
    rv <- dynGet("rv", list())
    txt <- paste0(before, middle, after, paste(..., sep = sep), end)
    txt <- crayon::reset(col(txt))
    base::message(txt, appendLF = FALSE)
  }
}

infomsg <- function(..., after = " INFO: ", col = green) {
  msg(..., after = after, col = col)
}

warnmsg <- function(..., after = " WARNING: ", col = yellow) {
  msg(..., after = after, col = col)
}

debugmsg <- function(..., after = " DEBUG: ", col = blue) {
  msg(..., after = after, col = col)
}

errormsg <- function(..., after = " ERROR: ", col = red) {
  msg(..., after = after, col = col)
}

# Helpers
now_ms <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS3")
}
