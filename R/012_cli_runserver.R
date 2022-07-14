cli_runserver <- function(argv) {
    docopt::docopt(cli_runserver_txt, args = argv)
    runserver()
}

cli_runserver_txt <- "Usage:
    runserver [-h]
Options:
    -h, --help   show this help and exit"
