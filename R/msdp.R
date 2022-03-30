#' @title Mean Standard Deviation Plot
#'
#' @description The mean against standard deviation for each feature in a list of datasets. The datasets must not contain non numeric columns.
#'
#' @param dd list(char), dataset names
#' @param xx list(data.frame), covariate dataframes (must contain only numeric columns)
#' @param xt char, how to transform the xaxis for plotting (options are 'identity', 'log2', 'log10' and 'rank')
#' @param yt char, how to transform the xaxis for plotting (options are 'identity', 'log2', 'log10' and 'rank')
#' @param ff vector(char), feature names to draw, if `is.none(ff)`, all features are drawn
#' @param sh char, character shape to use, not implemented yet
#'
#' @return TODO
#' @examples TODO
makeMSDPlot <- function(dd, xx, xt, yt, ff=NULL, shape=".") {
    # message(glue("making MSD plot for: {collapseCS(dd)} (xt={xt}, yt={yt})"))
    if (length(dd) == 0) {
        df <- data.frame(x=0, y=0, label="No dataset provided")
        ggplot(df, aes(x, y, label=label)) + geom_text() + theme_void()
    } else {
        if (!is.none(ff)) {
            xx2 <- map(xx, function(x) {
                cn <- colnames(x)
                ffexist <- intersect(ff, cn)
                ffna <- setdiff(ff, ffexist)
                x <- x[, ffexist]
                # x[, ffna] <- NA
                x
            })
        } else {
            xx2 <- xx
        }
        means <- nl_nv_2_df(lapply(xx2, function(x) apply(x, 2, mean)), col1="dataset", col2="feature", col3="mean")
        sds   <- nl_nv_2_df(lapply(xx2, function(x) apply(x, 2, sd)),   col1="dataset", col2="feature", col3="sd")
        # head(means); tail(means);                 head(sds); tail(sds);
        #           dataset   feature      mean                dataset  feature         sd
        #     1  lamis_train     RFC2  7.851505         1  lamis_train     RFC2  0.6243982
        #     2  lamis_train    HSPA6  5.784336         2  lamis_train    HSPA6  0.8377059
        # 20657     ghsg.set   WDR83   9.108829     20657     ghsg.set    WDR83  0.4851053
        # 20658     ghsg.set     WT1   7.029945     20658     ghsg.set      WT1  1.3353375
        data  <- merge(means, sds, by=1:2)
        # head(data); tail(data)
        #              dataset  feature       mean         sd
        #       1     ghsg.set      A2M  12.804626  0.7988832
        #       2     ghsg.set     ABAT   8.959297  0.6783653
        #   20657  lamis_train    ZZEF1   5.283571  0.2573278
        #   20658  lamis_train     ZZZ3   7.642813  0.3284888
        p <- ggplot(data=data, aes(x=mean, y=sd))
        p <- p + geom_point(aes(color=dataset), data) # shape=shape
        if (!is.none(ff)) {
            n <- length(ff)
            data$id <- as.numeric(as.factor(data$feature))
            p <- p + geom_text(aes(label=feature, col=dataset), hjust=-0.2, vjust=-0.2, size=3)
        }
        p <- p + coord_trans(x=xt, y=yt)
        print(p)
    }
}
