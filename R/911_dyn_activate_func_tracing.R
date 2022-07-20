get_pkg_funcs <- function() {
  r_dir <- system.file("R", package = "imlui")
  r_files <- dir(r_dir, "*.R", full.names = TRUE)
  r_code <- paste(sapply(r_files, readr::read_file), collapse = "\n")
  func_pattern <- "\\w+(?=\\s*<-\\s*function\\()"
  funcs <- stringr::str_extract_all(r_code, func_pattern)[[1]]
  funcs <- funcs[sapply(funcs, exists)]
  funcs <- funcs[sapply(funcs, \(x) is.function(get(x)))]
  return(funcs)
}


activate_func_tracing <- function() {
  lapply(get_pkg_funcs(), function(f) {
    try(trace(f, function() debugmsg("Entering", f), print=FALSE))
  })
}


traced <- function(func) {
  browser()
  func_name <- substitute(func)
  func_code <- as.character(body(func))
  trace_line <- glue("debugmsg('Entering', {func_name})")
  func_code_traced <- paste(trace_line, func_code, sep="\n")
  body(func) <- as.body(func_code_traced)
}

