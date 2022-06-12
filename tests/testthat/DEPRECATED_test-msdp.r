makeMSDPlotTests <- function() {
    ## INPUTS
    input <- II <- list(MM=c("lamis_signature"),
                        DD=c("lamis_train", "ghsg.set", "lamis_test1", "lamis_test2"),
                        DTCS=c("All"))
    # PASC  <- function() { II$PASC}
    # MPAW  <- function() { II$MPAW}
    # MPAH  <- function() { II$MPAH}
    # BW    <- function() { if (is.null(II$dim[1])) 640 else II$dim[1]}
    # BH    <- function() { if (is.null(II$dim[2])) 480 else II$dim[2]}
    MM    <- function() { if (!is.null(II$MM)) II$MM else list()}
    DD    <- function() { if (!is.null(II$DD)) II$DD else list()}
    DTCS  <- function() { II$DTCS}
    # MPW   <- function() { BW() *   0.75}
    # MPH   <- function() { BH() -  42.00}
    PP    <- function() { clapply(MM(), getdata)}
    XX    <- function() { clapply(DD(), getdata) }
    FF    <- function() { lapply(XX(), colnames)}
    SS    <- function() { lapply(XX(), rownames)}
    CXX   <- function() { lapply(XX(), function(X) dplyr::select(X,  where(is.factor)))}
    NXX   <- function() { lapply(XX(), function(X) dplyr::select(X, !where(is.factor)))}
    # APAW  <- function() { PAWS[max(which(PAWS < (MPW() -   0)), 1)]}
    # APAH  <- function() { PAHS[max(which(PAHS < (MPH() - 120)), 1)]}
    YYY   <- function() {
        lapply(PP(), function(P) {
            lapply(XX(), function(X) {
                tryCatch(
                    predict(P, X),
                    error=function(cond) { NULL }
                )
            })
        })
    } # YYY = Outcomes[model][dataset] = reactive(list(list(num)))

    # PAW   <- function() { if ( PASC() == "Fixed" && !(is.null(MPAW())) ) MPAW() else APAW()}
    # PAH   <- function() { if ( PASC() == "Fixed" && !(is.null(MPAH())) ) MPAH() else APAH()}
    # PAWPX <- function() { paste0(PAW(), "px")}
    # PAHPX <- function() { paste0(PAH(), "px")}


    dd <- DD()
    xx <- NXX()
    predictions <- YYY()[[MM()]]
}