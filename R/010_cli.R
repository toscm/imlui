#' @export
#' @title Commandline Interface
#' @description Entry point when called directly from a system shell such as
#' bash. I.e. calling imlui:::cli("--help") from an interactive R session is
#' equivalent to calling `R --no-echo --vanilla -q -e "imlui:::cli()" --help`
#' from bash.
#' @param argv character vector of commandline arguments
#' @examples sum(1:10); \dontrun{sum('a', 'b')}
#' @details To make shell usage more convenient, it is strongly recommended to
#' create an alias/wrapper for your shell of choice. To create an alias in
#' bash, add the line 'alias imlui=R --no-echo --vanilla -q -e "imlui:::cli()"'
#' to ~/.bashrc. To create an alias for CMD.exe, create a file "imlui.bat" in a
#' folder added to the PATH environment variable containing the line 'R
#' --no-echo --vanilla -q -e "imlui:::cli()" %*'.
#'
#' Now `imlui` can be used directly from commandline as in `imlui --help`.
#' @examples
#' cli(c("runserver"))
#' cli(c("--help"))
cli <- function(argv = commandArgs(trailingOnly = TRUE)) {
    version <- readLines(system.file("DESCRIPTION", package = "imlui")) %>%
        grep("Version:", ., value = T) %>%
        gsub("Version: ", "", .)
    args <- docopt::docopt(doc = cli_txt, args = argv, version = version)
    if (args$command == "runserver") {
        cli_runserver(argv = argv[-1])
    } else if (args$command == "visualize") {
        cli_visualize(argv = argv[-1])
    } else {
        msg <- paste0("'", argv[1], "' is not a valid command.")
        msg <- paste(msg, "See 'imlui --help'.")
        stop(msg, call. = FALSE)
    }
}

cli_txt <- "Usage:
    imlui (--help|--version)
    imlui <command> [<args>...]

Options:
    -h, --help   show this help and exit
    <command>    one of the commands decribed below
    <args>       additional arguments for <command>

Available commands:
    list        list available datasets and models
    runserver   start the webserver (GUI)
    visualize   visualize the given models and/or datasets

Use `imlui <command> -h` to print additional help for for <command>."
