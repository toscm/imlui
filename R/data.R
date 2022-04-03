#' The LAMIS signature
#'
#' The LAMIS signature as described in *Staiger, A.M., Altenbuchinger, M.,
#' Ziepert, M. et al. A novel lymphoma-associated macrophage interaction
#' signature (LAMIS) provides robust risk prognostication in diffuse large
#' B-cell lymphoma clinical trial cohorts of the DSHNHL. Leukemia 34, 543-552
#' (2020). https://doi.org/10.1038/s41375-019-0573-y*
#'
#' @format A named vector of length 17.
#' @examples
#' round(lamis_signature, 3)
#' #   CSF1  CPT1A  CD163   CCL5 BCL2A1  CXCR4    MME    BSG   CD47   IL16
#' # -0.685  0.685  0.155 -0.146 -0.134  0.122 -0.089  0.089  0.079 -0.065
#' #    FYN   MPST DPYSL3  CCND2  CPNE3    TBP  PSAT1
#' # -0.058  0.050 -0.049  0.040 -0.031  0.031  0.005
#' @source \url{https://www.nature.com/articles/s41375-019-0573-y/}
"lamis_signature"

#' The LAMIS training data
#'
#' The LAMIS training data as described in *Staiger, A.M., Altenbuchinger, M.,
#' Ziepert, M. et al. A novel lymphoma-associated macrophage interaction
#' signature (LAMIS) provides robust risk prognostication in diffuse large
#' B-cell lymphoma clinical trial cohorts of the DSHNHL. Leukemia 34, 543-552
#' (2020). https://doi.org/10.1038/s41375-019-0573-y*
#'
#' @format A data.frame with 233 obs. of 20524 variables. Columns 1-22
#' contain the following variables:
#' ```
#' 1. Gender [Levels: female, male]
#' 2. Age [Range: 17-92]
#' 3. Tissue [Levels: lymphnode]
#' 4. Disease [Levels: Lymphoma]
#' 5. Individual [Range: 2-2814]
#' 6. diagnosis [Levels: DLBCL]
#' 7. ABCGCB [Levels: ABCDLBCL, GCBDLBCL, UnclassifiedDLBCL]
#' 8. Follow.up.status [Levels: ALIVE, DEAD]
#' 9. Follow.up.years [Range: 0-10.29]
#' 10. Chemotherapy [Levels: R-CHOP-LikeRegimen]
#' 11. ECOG.performance.status [Levels: 0, 1, 2, 3, 4]
#' 12. Stage [Levels: 1, 2, 3, 4]
#' 13. LDH.ratio [Range: 0-30.76]
#' 14. extranodal.sites [Levels: 0, 1, 2, 3, 4, 5]
#' 15. IPI.scores [Levels: 0, 1, 2, 3, 4, 5]
#' 16. IPI.rating [Levels: 0, 1, 2]
#' 17. ABCGCB.score [Levels: 0, 0.52, 1]
#' 18. IPI_AGE [Levels: 0, ]
#' 19. IPI_LDH [Levels: 0, 0.50, 1]
#' 20. IPI_ECOG [Levels: 0, 0.23, 1]
#' 21. IPI_STAGE [Levels: 0, 0.39, 1]
#' 22. IPI_EXBM [Levels: 0, 0.07, 1]
#' ```
#' Columns 23-20524 contain log2 transformed gene expression values.
#'
#' @examples
#' lamis_train[1:3, 1:25]
#' #           Gender Age    Tissue  Disease Individual diagnosis   ABCGCB
#' # GSM275076   male  73 lymphnode Lymphoma       1179     DLBCL ABCDLBCL
#' # GSM275077 female  71 lymphnode Lymphoma       1181     DLBCL GCBDLBCL
#' # GSM275078   male  71 lymphnode Lymphoma       1184     DLBCL GCBDLBCL
#' #           Follow.up.status Follow.up.years       Chemotherapy
#' # GSM275076            ALIVE            2.52 R-CHOP-LikeRegimen
#' # GSM275077            ALIVE            1.71 R-CHOP-LikeRegimen
#' # GSM275078            ALIVE            4.74 R-CHOP-LikeRegimen
#' #           ECOG.performance.status Stage LDH.ratio extranodal.sites
#' # GSM275076                       0     4      0.92                1
#' # GSM275077                       1     2      0.63                0
#' # GSM275078                       0     3      0.74                0
#' #           IPI.scores IPI.rating ABCGCB.score IPI_AGE IPI_LDH IPI_ECOG
#' # GSM275076          2          1            0       1       0        0
#' # GSM275077          1          0            1       1       0        0
#' # GSM275078          2          1            1       1       0        0
#' #           IPI_STAGE IPI_EXBM     RFC2    HSPA6     PAX8
#' # GSM275076 1.0000000        0 8.059630 5.399721 4.788501
#' # GSM275077 0.0000000        0 8.578449 6.115090 4.977569
#' # GSM275078 0.3915858        0 7.934017 5.191435 5.064762
#' @source \url{https://www.nature.com/articles/s41375-019-0573-y/}
"lamis_train"

#' The LAMIS test data 1 (validaton set I).
#'
#' The LAMIS test data as described in *Staiger, A.M., Altenbuchinger, M.,
#' Ziepert, M. et al. A novel lymphoma-associated macrophage interaction
#' signature (LAMIS) provides robust risk prognostication in diffuse large
#' B-cell lymphoma clinical trial cohorts of the DSHNHL. Leukemia 34, 543-552
#' (2020). https://doi.org/10.1038/s41375-019-0573-y*. Called *validation set
#' I* in the paper.
#'
#' @format A data.frame with 181 obs. of 20524 variables. Columns 1-22
#' contain the following variables:
#' ```
#' 1. Gender [Levels: female, male]
#' 2. Age [Range: 14-88]
#' 3. Tissue [Levels: lymphnode]
#' 4. Disease [Levels: Lymphoma]
#' 5. Individual [Range: 304-1061]
#' 6. diagnosis [Levels: DLBCL]
#' 7. ABCGCB [Levels: ABCDLBCL, GCBDLBCL, UnclassifiedDLBCL]
#' 8. Follow.up.status [Levels: ALIVE, DEAD]
#' 9. Follow.up.years [Range: 0-21.78]
#' 10. Chemotherapy [Levels: CHOP-LikeRegimen]
#' 11. ECOG.performance.status [Levels: 0, 1, 2, 3, 4]
#' 12. Stage [Levels: 1, 2, 3, 4]
#' 13. LDH.ratio [Range: NA-NA]
#' 14. extranodal.sites [Levels: 0, 1]
#' 15. IPI.scores [Levels: 0, 1, 2, 3, 4]
#' 16. IPI.rating [Levels: 0, 1, 2]
#' 17. ABCGCB.score [Levels: 0, 0.52, 1]
#' 18. IPI_AGE [Levels: 0, 1]
#' 19. IPI_LDH [Levels: 0, 0.51, 1]
#' 20. IPI_ECOG [Levels: 0, 0.24, 1]
#' 21. IPI_STAGE [Levels: 0, 0.39, 1]
#' 22. IPI_EXBM [Levels: 0, 0.08]
#' ```
#' #' Columns 23-20524 contain log2 transformed gene expression values.
#' @examples
#' lamis_test1[1:3, 1:25]
#' #           Gender Age    Tissue  Disease Individual diagnosis   ABCGCB
#' # GSM274895   male  52 lymphnode Lymphoma        549     DLBCL GCBDLBCL
#' # GSM274896   male  75 lymphnode Lymphoma        553     DLBCL GCBDLBCL
#' # GSM274897 female  76 lymphnode Lymphoma        689     DLBCL ABCDLBCL
#' #           Follow.up.status Follow.up.years     Chemotherapy
#' # GSM274895             DEAD            2.68 CHOP-LikeRegimen
#' # GSM274896             DEAD            0.82 CHOP-LikeRegimen
#' # GSM274897             DEAD            2.54 CHOP-LikeRegimen
#' #           ECOG.performance.status Stage LDH.ratio extranodal.sites
#' # GSM274895                       1     3        NA                0
#' # GSM274896                       0     3      1.12                0
#' # GSM274897                       3     1      1.01                0
#' #           IPI.scores IPI.rating ABCGCB.score IPI_AGE   IPI_LDH IPI_ECOG
#' # GSM274895         NA         NA            1       0 0.5071225        0
#' # GSM274896          3          1            1       1 1.0000000        0
#' # GSM274897          3          1            0       1 1.0000000        1
#' #           IPI_STAGE IPI_EXBM     RFC2    HSPA6     PAX8
#' # GSM274895 0.3915858        0 7.080452 6.478552 4.987939
#' # GSM274896 0.3915858        0 7.924949 6.470217 5.111094
#' # GSM274897 0.0000000        0 7.188202 6.206049 5.114223
#' @source
#' \url{https://www.nature.com/articles/s41375-019-0573-y/}
"lamis_test1"
