cli_visualize <- function(argv) {
    args <- docopt::docopt(cli_visualize_txt, args = argv)
    visualize(model = args$model, data = args$data)
}

cli_visualize_txt <- "Usage:
    visualize [-h] -m <model> -d <dataset>
Options:
    -h, --help            show this help and exit
    -m, --model <model>   model to visualize
    -d, --data <dataset>  dataset to visualize"
