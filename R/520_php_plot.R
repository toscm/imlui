#' @export
#' @name plot_predictions_histogram
#' @title Plot Histogram of Predictions
#' @description Return ggplot2 object of histogram of Predictions
#' @param predictions as the name says (named list of named numeric vectors)
#' @param density_lines add density lines to the plot (logical)? (logical)
#' @param rug add rugs to the plot? (logical)
#' @param binwidth binwidth for histogram
#' @return ggplot2 object
plot_predictions_histogram <- function(predictions,
                                       density_lines = TRUE,
                                       rug = TRUE,
                                       binwidth = NULL) {

    # TODO: remove tidyverse NSE shit to fix warnings below. Then remove stubs...
    # plot_predictions_histogram: no visible binding for global variable 'y'
    # plot_predictions_histogram: no visible binding for global variable 'dataset'
    y <- dataset <-  NULL
    # END TODO

    # str(predictions)
    # $ lamis_train: c(GSM275076=-0.68, ..., GSM275377=+1.00) 233 samples
    # $ lamis_test1: c(GSM274895=-0.50, ..., GSM275196=-1.54) 181 samples
    df <- nl_nv_2_df(predictions)
    # head(df)
    #         dataset    sample     y
    #   1 lamis_train GSM275076 -0.68
    # ...         ...       ...   ...
    # 233 lamis_train GSM275377 +1.00
    # 234 lamis_test1 GSM274895 -0.50
    # ...         ...       ...   ...
    # 414 lamis_test1 GSM275196 -1.54
    binwidth <- binwidth %||% (ceiling10(diff(range(df$y))) / 100)
    p <- ggplot(data = df, mapping = aes(x = y))
    if (density_lines) {
        p <- p + geom_density(mapping = aes(col = dataset), size = 1)
    } else {
        p <- p + geom_histogram(
            mapping = aes(fill = dataset, col = dataset),
            binwidth = binwidth,
            alpha = 0.2,
            size = 1,
            position = "identity"
        )
    }
    if (rug) p <- p + geom_rug(aes(color = dataset))
    print(p)
}