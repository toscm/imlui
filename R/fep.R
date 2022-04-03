#' @export
#' @name plot_feature_effects
#' @title Visualize Feature Effects
#' @description Visualize feature effects for linear models of the form  y = X * b
#' @param X dataset (n*m matrix)
#' @param b betas of linear model (named m*1 vector)
#' @param s sample of interest
#' @param debug draw colored borders to debug the plot
#' @param n_extreme how many samples from X to draw into the plot?
#' Samples are taken from both end of the y spectrum, i.e.
#' @param threshold threshold for classification setting
#' @return `par(opar)``
plot_feature_effects <- function(
        b,
        X,
        s,
        debug=FALSE,
        n_extreme=10,
        threshold=0.991
    ) {
    # User Info
    # caterr("Normal caterr")
    # PRINT("Calling plot_feature_effects(b, XX, s, debug, n_extreme, threeshold) with:")
    # PRINT("b=II$MM[[1]]")
    # PRINT(II$MM[[1]])
    # PRINT("XX=XX()[[1]]")
    # PRINT(XX()[[1]][1:3,1:3])
    # PRINT("s=II$FET_S %||% SS()[[1]][[1]])")
    # PRINT(II$FET_S %||% SS()[[1]][[1]])
    # PRINT("debug=II$debug")
    # PRINT(II$debug)
    # PRINT("n_extreme=II$n_extreme")
    # PRINT(II$n_extreme)
    # PRINT("threshold=II$threshold")
    # PRINT(II$threshold)

    # Init symbols
    si <- s # sample of interest

    b_pos <- sort(b[b >= 0], decreasing=TRUE)
    b_neg <- sort(b[b <  0])
    b <- c(b_pos, b_neg)
    if (all(names(b) %in% colnames(X))) {
        Xt <- X
        X <- t(X[, names(b)])
        # TODO: switch this. Features in cols is correct! So the other
        # should be transposed and therefore slower...
    } else if (all(names(b) %in% rownames(X))) {
        Xt <- t(X)
        X <- X[names(b), ]
    } else {
        stop("names(b) are no subset of colnames(X)")
    }
    X_pos <- X[names(b_pos), ]
    X_neg <- X[names(b_neg), ]
    y_pos <- (t(X_pos) %*% b_pos)[, 1]
    y_neg <- (t(X_neg) %*% b_neg)[, 1]
    y <- (t(X) %*% b)[, 1]
    ticks <- data.frame("right" =  c(cumsum(abs(b))))
    ticks$left <- c(0, ticks$right[-length(ticks$right)])
    fig <- data.frame("signature" = (c(0, 8, 0, 10) / 10))
    fig$score <- (c(8, 10, 0, 10) / 10)
    mar <- data.frame("signature" = (c(2, 3, 0, 0) + 0.1))
    mar$score <- (c(2, 3, 0, 0) + 0.1)
    x_max <- max(ticks$right)
    y_max <-  ceiling(max(X))

    # Global Settings
    opar <-  par(no.readonly = TRUE)
    par(opar)  # oma=c(b, l, t, r), mfrow=c(1, 2), bty="n"
    par(xaxs="i", yaxs="i", xaxt="n", oma=(c(0, 0, 2, 0)))

    # Signature area
    par(fig=fig$signature, mar=mar$signature)
    plot(
        x=NA, y=NA,
        xlim=c(0, x_max),
        ylim=c(0, y_max),
        xlab=NA, ylab=NA,
        yaxt="s"
    )
    mtext("Expression", side=2, line=1.5)
    mtext("Genes ", side=1, line=0.5)
    # Grid and gene names
    abline(v=ticks$left)
    idx <- (abs(b) >= 0.03)
    text(
        x=ticks$left[idx],
        y=(1),
        labels=paste0(names(b[idx]), " (", round(b[idx], 2), ")"),
        srt=90,
        pos=1
    )

    # Draw lines for samples with lowest/highest score (in blue/red)
    y_sort <- sort(y)
    s <- names(y_sort) # sample names (sorted by score)
    n <- length(y)
    s_lowest <- s[1:n_extreme]
    s_highest <- s[(n - n_extreme + 1):n]
    if (n_extreme >= 1) {
        for (s in c(s_lowest, s_highest)) {
            col <- if (s %in% s_lowest) "blue" else "red"
            # shortname <- substr(s, nchar(s) -3, nchar(s))
            for (i in 1:length(b)) {
                f <- names(b)[i] # f == feature name
                lines(
                    x=c(ticks$left[i], ticks$right[i]),
                    y=c(X[f, s], X[f, s]),
                    col=col
                )
                # text(
                #     x=ticks$left[i],
                #     y=gep[gene],
                #     labels=shortname,
                #     cex=0.66,
                #     col=col,
                #     pos=4
                # )
            }
        }
    }

    # Sample of interest
    gep <- X[, si]
    names(gep) <- rownames(X)
    score <- y[si]
    col="orange"
    shortname <- substr(si, nchar(si) -3, nchar(si))
    for (i in 1:length(b)) {
        gene <- names(b)[i]
        weight <- b[i]
        ge <- gep[gene]
        lines(
            x=c(ticks$left[i], ticks$right[i]),
            y=c(gep[gene], gep[gene]),
            col=col, lwd=10
        )
        # Red rectangles --> increase score --> increase risk
        rect(
            xleft=ticks$left[i],
            ybottom=ifelse(
                weight >= 0,
                ge + ((threshold - score) / weight),
                0
            ),
            xright=ticks$right[i],
            ytop=ifelse(
                weight >= 0,
                y_max,
                ge + ((threshold - score) / weight)
            ),
            col ="#ff000020"
        )
        # Blue rectangles --> reduce score --> reduce risk
        rect(
            xleft=ticks$left[i],
            ybottom=ifelse(
                weight >= 0,
                0,
                ge + ((threshold - score) / weight)
            ),
            xright=ticks$right[i],
            ytop=ifelse(
                weight >= 0,
                ge + ((threshold - score) / weight),
                y_max
            ),
            col ="#0000ff20"
        )
    }
    legend("topleft", legend=si, fill=col, col=col)

    # Score area
    par(fig=fig$score, new=TRUE, mar=mar$score)
    plot(
        x=NA, y=NA,
        yaxt="s",
        xlim=c(0, 3), ylim=c(
            floor(min(y)),
            ceiling(max(c(y_pos, y_neg)))
        ),
        xlab=NA, ylab=NA
    )
    abline(v=c(1,2))
    mtext("Score ", side=1, line=0.5)
    # for (sample in c(s_lowest, s_highest)) {
    #     score_pos <- y_pos[sample]
    #     score_neg <- y_neg[sample]
    #     score <- y[sample]
    #     col=ifelse(sample %in% s_lowest, "blue", "red")
    #     shortname <- substr(sample, nchar(sample) -3, nchar(sample))
    #     lines(x=c(0, 1), y=c(score_pos, score_pos), col=col)
    #     text(x=0, y=score_pos, labels=shortname, cex=0.66, col=col, pos=4)
    #     lines(x=c(1, 2), y=-c(score_neg, score_neg), col=col)
    #     text(x=1, y=-score_neg, labels=shortname, cex=0.66, col=col, pos=4)
    #     lines(x=c(2, 3), y=c(score, score), col=col)
    #     text(x=2, y=score, labels=shortname, cex=0.66, col=col, pos=4)
    # }
    lines(x=c(2, 3), y=c(threshold, threshold), col="black", lwd=3)
    # Sample of interest
    gep <- X[, si]; names(gep) <- rownames(X)
    score <- y[si]
    col="orange"
    shortname <- substr(si, nchar(si) -3, nchar(si))
    rect(xleft=0, ybottom=0, xright=1, ytop=y_pos[si], col ="#ff000040")
    rect(xleft=1, ybottom=0, xright=2, ytop=-y_neg[si], col ="#0000ff40")
    rect(xleft=2, ybottom=0, xright=3, ytop=score[si], col ="#DF902040")

    # Outer Area
    mtext("Feature Effects", side=3, line=0.5, outer=TRUE)

    # Debug Boxes
    if (debug) {
        draw_debug_boxes(fig=fig$score, mar=mar$score)
    }

    par(opar)
}

draw_debug_boxes <- function(
        fig=NA, mar=NA, pcol="red", fcol="green", ocol="blue"
    ) {
    # Signature
    if (any(!is.na(fig))) {
        par(fig=fig, mar=mar, new=TRUE)
        for (side in seq(4)) {
            if (mar[side] >= 1) {
                for (line in seq(0, mar[side])) {
                    mtext(line, side=side, line=line, adj=1, col=pcol)
                    box(which="plot", col=pcol)
                }
            }
        }
    }
    if (!is.na(pcol)) { box(which="plot", col=pcol) }
    if (!is.na(fcol)) { box(which="figure", col=fcol) }
    if (!is.na(ocol)) { box(which="outer", col=ocol) }
}

# GENELIST
# par(fig=(c(8, 10, 5, 10) / 10), new=TRUE, mar=c(0, 0, 2, 0))
# plot(x=NA, y=NA, xlim=c(0, 10), ylim=c(0, length(b)), xlab=NA, ylab=NA)
# mtext("0", side=3, line=0, adj=1, col="green")
# mtext("1", side=3, line=1, adj=1, col="green")
# mtext("Genelist", side=3, line=0.5)
# box(which="figure", col="green")
# box(which="plot", col="red")

# PLOT args
# main: main title of the plot (plot)

# PAR args
# adj: (0|1)=(left|right)-justify text, mtext and title (0.5=center)
# ann: F = disable annotation of plots with axis- and overall titles
# bg: background color (default: "white")
# bty: Box type ("y"=default, "l", "7", "c", "u", "]", "n"=no box)
# cex: Text and symbol magnification relative to default (0-Inf)
# cexxis: Axis magnification relative to `cex'
# cexab: Label magnification relative to `cex'
# cexain: Main title
# cexub: Sub title
# col: default plotting color
# colxis: axis annotation color
# colab: x and y label color
# colain: main title color
# colub: sub-title color
# cra: character size c(b, h) in pixels
# (srt|crt): string/character roation in degrees (0-360)
# c(in|si|xy): char size in (inches|inches|coord-units) as c(widht, height)
# cxy: character size c(b, h) in user coordinate units
# din: device dimensions c(b, h) in inches
# family: The name of a font family for drawing text
# fg: default foreground color
# fig: display region coordinates for next plot.new c(x1, x2, y1, y2)
# fin: The figure region dimensions c(b, h) in inches
# font: 1=normal, 2=bold, 3=italic, 4=bold italic
# font(xis|ab|ain|ub): (axis-annotation|label|main-title|sub-title)-font
# lab: number of [xy]-ticks and label length as c(x, y, len) [c(5, 5, 7)]
# las: style of axis labels (numeric in {0,1,2,3})
# lend: line end style (integer or string)
# lheight: line height multiplier
# ljoin: line join style
# lmitre: line mitre limit
# lty: line type "(blank|solid|dashed|dotted|dotdash|longdash|twodash)"
# lwd: line width (default: 1)
# mai: margin size in inches as c(b, l, t, r)
# mar: margin lines as c(b, l, t, r) [c(5.1, 4.1, 4.1, 2.1)]
# mex: margin character size expansion factor
# mf(col|row): plot by (cols|rows) in a grid of size c(nr, nc)
# mfg: which figure to draw next c(i, j)
# mgp: margin line (mex units) for axis-(title|labels|line) [c(3, 1, 0)]
# new: TRUE = do not clean frame at next call to plot.new
# oma: size of outer margins in lines of text as c(b, l, t, or)
# omd: size of outer margins in normalized dev coords as c(x1, x2, y1, y2)
# omi: size of outer margins in inches as c(b, l, t, r)
# page: should next plot.new start a new page [FALSE]
# pch: default plotting symbol (int or single char)
# pin: plot dimensions c(b, h), in inches.
# plt: plot region as fraction of figure region as c(x1, x2, y1, y2)
# ps: point size of text (integer)
# pty: plot region type ("s"=square, "m"=maximal plotting region)
# tck: tick mark length as fraction of plotting region [NA: use tcl = -0.5]
# tcl: tick mark length as fraction of line height [-0.5]
# usr: plotting region coordinates as c(x1, x2, y1, y2)
# [xy]axp: coords of extreme tick marks and number of intervals inbetween
# [xy]axs: axis interval calculation style "(r|i|e|s|d)"
# [xy]axt: axis type ("n"=no plotting) ["s"]
# [xy]log: TRUE= use log scale
# xpd: (FALSE|TRUE|NA) = clip plotting to (plot|figure|device)-region
