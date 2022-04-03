########## FULL IMPORTS ################################################################################################
#' @import configr
#' @import ggplot2
#' @import purrr
#' @import shiny
#' @import shinyWidgets
#' @import shinydashboard
#' @import stringr
#' @import svglite
#' @import tibble

########## SELECTIVE IMPORTS ###########################################################################################
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

########## GLOBALS TO SHADOW TIDYVERSE NON STANDARD EVALUATION SHIT ####################################################
# TODO: Remove all tidyverse functions that use NSE, because it causes R CMD CHECK warnings. For now we added globals
# to trick R CMD CHECK, but thats no permanent solution.
where <- function(...) {stop("This function should never be called!")}

########## DEFINE GLOBAL DOT TO MAKE MAGRITTR PIPE WORK ################################################################
. <- NULL

########## MAKE SURE FILE IS NOT EMPTY #################################################################################
{} # required so devtools::document() recognizes the file...
