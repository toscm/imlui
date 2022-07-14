# Full Imports
#' @import configr
#' @import ggplot2
#' @import purrr
#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
#' @import stringr
#' @import svglite
#' @import tibble

# Selective Imports
#' @importFrom dplyr rename select
#' @importFrom glue glue
#' @importFrom graphics abline legend lines mtext par rect text
#' @importFrom grDevices dev.off
#' @importFrom magrittr %>%
#' @importFrom rlang env env_names env_print new_environment
#' @importFrom stats predict sd
#' @importFrom tibble as_tibble
#' @importFrom utils str
#' @importFrom utils capture.output data help packageVersion str
#' @importFrom shinycssloaders withSpinner
#' @importFrom memoise memoise
#' @importFrom toscmask * +

# Globals to shadow tidyverse NSE shit (TODO: remove)
where <- function() stop("This function should never be called!")
. <- function() stop("This function should never be called!")

# Make sure file is not empty so devtools::document() recognizes the file
{}
