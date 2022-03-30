cli_doc <-  "Usage:
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
cli <- function(argv=commandArgs(trailingOnly=TRUE)) {
    # options(warn=1)  # show warnings as soon as they occur
    # source("util.R")
    # log <- file.path(getfd(), "..", "log", "imlui.log")
    # close(file(log, open="w")) # clear log file
    # libraries(c("docopt"), log=log)
    version <- readLines(system.file("DESCRIPTION", package="imlui")) %>%
        grep("Version:", ., value=T) %>%
        gsub("Version: ", "", .)
    args <- docopt::docopt(doc=cli_doc, args=argv, version=version)
    if (args$command == "runserver") {
        runserver_cli(argv=argv[-1])
    } else if (args$command == "visualize") {
        visualize_cli(argv=argv[-1])
    } else {
        msg <- paste0("'", argv[1], "' is not a valid command.")
        msg <- paste(msg, "See 'imlui --help'.")
        stop(msg, call. = FALSE)
    }
    # TODO: do something like the following
    # if (args$command %in% c("list", "runserver", "visualize")) {
    #     func <- paste0("imlui:::", args$command, "_cli")
    #     get(func, args[-1])
    # }
    # cat("calling: ", func, "(", sep="")
    # cat(argv[-1], sep=", ")
    # cat(")\n", sep="")
}



runserver_cli_doc <- "Usage:
    runserver [-h]
Options:
    -h, --help   show this help and exit"
# Commandline Interface
#
# Entry point when called directly from a system shell such as bash. I.e.
# calling imlui:::cli("--help") from an interactive R session is equivalent
# to calling `R --no-echo --vanilla -q -e "imlui:::cli()" --help` from bash.
#
# To make shell usage more convenient, it is strongly recommended to create an
# alias/wrapper for your shell of choice. To create an alias in bash, enter
# `alias imlui=R --no-echo --vanilla -q -e "imlui:::cli()`. To create an
# alias for CMD.exe, create a file "imlui.bat" in a folder added to the PATH
# environment variable containing the line `R --no-echo --vanilla -q -e
# "imlui:::cli()" %*`
#
# Now `imlui` can be used directly from commandline as in `imlui --help`.
# @param argv character vector of commandline arguments
# @examples
# cli(c("runserver"))
# cli(c("--help"))
# @export
runserver_cli <- function(argv) {
    docopt:::docopt(runserver_cli_doc, args=argv)
    runserver()
}


visualize_cli_doc <- "Usage:
    visualize [-h] -m <model> -d <dataset>
Options:
    -h, --help            show this help and exit
    -m, --model <model>   model to visualize
    -d, --data <dataset>  dataset to visualize"
visualize_cli <- function(argv) {
    args <- docopt:::docopt(visualize_cli_doc, args=argv)
    imlui:::visualize(model = args$model, data = args$data)
}
