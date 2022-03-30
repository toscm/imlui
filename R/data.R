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
#' round(imlui::lamis_signature, 3)
#'   CSF1  CPT1A  CD163   CCL5 BCL2A1  CXCR4    MME    BSG   CD47   IL16
#' -0.685  0.685  0.155 -0.146 -0.134  0.122 -0.089  0.089  0.079 -0.065
#'    FYN   MPST DPYSL3  CCND2  CPNE3    TBP  PSAT1
#' -0.058  0.050 -0.049  0.040 -0.031  0.031  0.005
#' @source \url{https://www.nature.com/articles/s41375-019-0573-y/}
"lamis_signature"



#' miRNA Overall Survival signature for DLBCL patients
#'
#' The overall survival signature described in Nordmo, Carmen, Gunther Glehr,
#' Michael Altenbuchinger, Rainer Spang, Marita Ziepert, Heike Horn, Annette M.
#' Staiger, et al. "Identification of a MiRNA Based Model to Detect Prognostic
#' Subgroups in Patients with Aggressive B-Cell Lymphoma". Leukemia & Lymphoma
#' 62, no. 5 (16 April 2021): 1107-15.
#' https://doi.org/10.1080/10428194.2020.1861268.
#'
#' @format A named vector of length 6.
#' @examples
#' nordmo_os_signature = c(
#'     "hsa_miR-106b-5p" = +0.189710,
#'     "hsa_miR-130a-3p" = -0.233681,
#'     "hsa_miR-186-5p"  = +0.069890,
#'     "hsa_miR-374b-5p" = +0.003091,
#'     "hsa_miR-423-5p"  = -0.069890,
#'     "hsa_miR-590-5p"  = +0.040880
#' )
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
"nordmo_os_signature"



#' miRNA Progression Free Survival signature for DLBCL patients
#'
#' The progression free survival signature described in Nordmo, Carmen, Gunther
#' Glehr, Michael Altenbuchinger, Rainer Spang, Marita Ziepert, Heike Horn,
#' Annette M. Staiger, et al. "Identification of a MiRNA Based Model to Detect
#' Prognostic Subgroups in Patients with Aggressive B-Cell Lymphoma". Leukemia
#' & Lymphoma 62, no. 5 (16 April 2021): 1107-15.
#' https://doi.org/10.1080/10428194.2020.1861268.
#'
#' @format A named vector of length 6.
#' @examples
#' nordmo_pfs_signature = c(
#'     "miR-106b-5p" = +0.967200,
#'     "miR-130a-3p" = -0.153873,
#'     "miR-365a-3p" = -0.945600,
#'     "miR-374a-5p" = +0.098353,
#'     "miR-423-5p"  = -0.138210,
#'     "miR-590-5p"  = +0.172130
#' )
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
"nordmo_pfs_signature"



#' Training data for miRNA survival signatures of DLBCL patients
#'
#' The (already log2 transformed) training data used in Nordmo, Carmen, Gunther
#' Glehr, Michael Altenbuchinger, Rainer Spang, Marita Ziepert, Heike Horn,
#' Annette M. Staiger, et al. "Identification of a MiRNA Based Model to Detect
#' Prognostic Subgroups in Patients with Aggressive B-Cell Lymphoma". Leukemia
#' & Lymphoma 62, no. 5 (16 April 2021): 1107-15.
#' https://doi.org/10.1080/10428194.2020.1861268.
#'
#' @format A 209 x 800 Dataframe
#' ```
#'        hsa-miR-892a hsa-miR-128 ... hsa-miR-454-3p hsa-miR-4485
#' S14901     4.087463    6.022368 ...       5.247928     4.523562
#' S15201     5.129283    5.357552 ...       6.129283     4.643856
#'    ...          ...         ... ...            ...          ...
#' S18101     4.087463    4.807355 ...       5.426265     3.807355
#' S18201     3.459432    4.807355 ...       5.000000     4.169925
#' ```
#' @source \url{https://doi.org/10.1080/10428194.2020.1861268}
"nordmo_pfs_signature"



#' Wolframs Mouse Data
#'
#' Protein profiles of mouses with different amounts of livers treated
#' with different drugs (H20 or EMPA).
#'
#' @format A data.frame with 28 obs. of 1962 variables. Column 1 (drug)
#' describes the mouse's treatment (EMPA or H20) and column 2 (livers)
#' the mouses amount of livers (probably Sh means 1 and UNx means 2,
#' but not sure about that... TODO). Columns 3-1962 describe protein
#' levels:
#' ```
#' 1. drug [Levels: EMPA, H2O]
#' 2. livers [Levels: Sh, UNx]
#' 3. sp|A2AJL3|FGGY_MOUSE [Range: 121816-332546]
#' 4. sp|A2AKK5|ACNT1_MOUSE [Range: 156999-386092]
#' 5. sp|A2ALS5|RPGP1_MOUSE [Range: 7587-96635]
#' 6. sp|A2APT9|KLD7A_MOUSE [Range: 4224-41638]
#' 7. sp|A2ARV4|LRP2_MOUSE [Range: 3855440-9426737]
#' 8. sp|A2ASQ1|AGRIN_MOUSE [Range: 11294-109993]
#' 9. sp|A2ATU0|DHTK1_MOUSE [Range: 42728-172820]
#' 10. sp|A3KMP2|TTC38_MOUSE [Range: 195289-460792]
#' 11. sp|B1AVD1|XPP2_MOUSE [Range: 39261-217552]
#' 12. sp|B2RSH2|GNAI1_MOUSE [Range: 14384-219362]
#' 13. sp|B9EJ86|OSBL8_MOUSE [Range: 93481-278556]
#' 14. sp|D3Z7P3|GLSK_MOUSE [Range: 148542-325257]
#' 15. sp|E9PVA8|GCN1_MOUSE [Range: 4658-218314]
#' 16. sp|E9Q3L2|PI4KA_MOUSE [Range: 11597-160966]
#' 17. sp|E9Q557|DESP_MOUSE [Range: 94059-1126995]
#' 18. sp|E9Q612|PTPRO_MOUSE [Range: 22785-138597]
#' 19. sp|E9Q634|MYO1E_MOUSE [Range: 21909-144416]
#' 20. sp|G3X9C2|FBX50_MOUSE [Range: 56436-272938]
#' 21. sp|G5E829|AT2B1_MOUSE [Range: 26444-81168]
#' 22. sp|G5E8K5|ANK3_MOUSE [Range: 187914-613215]
#' 23. sp|O08529|CAN2_MOUSE [Range: 56052-221873]
#' 24. sp|O08547|SC22B_MOUSE [Range: 51032-189268]
#' 25. sp|O08553|DPYL2_MOUSE [Range: 63367-902331]
#' ```
#' @examples
#' r$> wolframs_mouse_data[,1:5]
#'        drug livers sp|A2AJL3|FGGY_MOUSE sp|A2AKK5|ACNT1_MOUSE sp|A2ALS5|RPGP1_MOUSE
#' M29651  H2O     Sh               311029                225797                 22829
#' M29613  H2O     Sh               196401                296919                 25143
#' M29611  H2O     Sh               163090                312092                 11310
#' M29691  H2O     Sh               179499                386092                 11655
#' M29631  H2O     Sh               168107                275513                  7587
#' M29693  H2O    UNx               214390                229509                 19347
#' M29711  H2O    UNx               176614                321788                 30677
#' M29632  H2O    UNx               156334                232193                 16637
#' M29712  H2O    UNx               332546                220514                 48960
#' M29641 EMPA     Sh               180234                156999                 29859
#' M29662 EMPA     Sh               197679                319568                 18867
#' M29721 EMPA     Sh               235326                245869                 37981
#' M29621 EMPA     Sh               163605                269223                 31367
#' M29701 EMPA     Sh               181955                310191                 35756
#' M29622 EMPA    UNx               227559                306449                 17950
#' M29722 EMPA    UNx               275727                306962                 20389
#' M29623 EMPA    UNx               128854                380689                  8912
#' M29642 EMPA    UNx               239226                307414                 24241
#' M29672  H2O     Sh               219573                202160                 39417
#' M29671  H2O     Sh               121816                214691                 24686
#' M29652  H2O    UNx               291227                361385                 51186
#' M29731  H2O    UNx               252188                312156                 96635
#' M29692  H2O    UNx               205545                338143                 84372
#' M29681 EMPA     Sh               169352                246204                 17015
#' M29653 EMPA     Sh               180709                252661                 81127
#' M29682 EMPA     Sh               213966                284455                 24757
#' M29702 EMPA    UNx               171259                310694                 29816
#' M29661 EMPA    UNx               306009                315091                 45640
#' @source \url{https://www.spang-lab.de/people/lbuck}
"wolframs_mouse_data"



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
#' imlui::lamis_train[1:3, 1:25]
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
#' imlui::lamis_test1[1:3, 1:25]
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



#' The LAMIS test data 2 (validaton set II).
#'
#' The LAMIS test data as described in *Staiger, A.M., Altenbuchinger, M.,
#' Ziepert, M. et al. A novel lymphoma-associated macrophage interaction
#' signature (LAMIS) provides robust risk prognostication in diffuse large
#' B-cell lymphoma clinical trial cohorts of the DSHNHL. Leukemia 34, 543-552
#' (2020). https://doi.org/10.1038/s41375-019-0573-y*. Called *validation set
#' II* in the paper.
#'
#' @format A data.frame with 466 obs. of 168 variables. Columns 1-23 contain
#' the following variables:
#' ```
#'
#' 1. STUDIDn: study identification number [7:RICOVER-60, 70:RICOVER-noRTh,
#'    9:MegaCHOEP Phase-III, 90:MegaCHOEP observation Phase-II, 12:DENSER,
#'    120:SMARTER, 999:MInT]
#' 2. PATSTUID: patient identification number within trial [Levels: 1, 100,
#'    101, 1013, ..., D-70-18, D-70-22, D-70-25, D-70-3, D-70-8]
#' 3. ARMID: TODO [Levels: 1, 3, 4, 5, 22]
#' 4. GENDER: gender [0:male, 1:female]
#' 5. AGE: age in years
#' 6. IPI_AGE: IPI factor age [0:age<=60, 1:age>60]
#' 7. IPI_LDH: IPI factor LDH [0:LDH<=N, 1:LDH>N]
#' 8. IPI_ECOG: IPI factor ECOG>1 [0:ECOG<=1, 1:ECOG>1]
#' 9. IPI_STAGE: IPI factor stage III/IV [0=stage I/II, 1=stage III/IV]
#' 10. IPI_EXBM: IPI factor extralymphatic involvement (EL) [0:EL<=1, 1:EL>1]
#' 11. IPI: IPI score [0-5]
#' 12. AA_IPI: age-adjusted (< 60 years) IPI score [0-3]
#' 13. EXBM: extralymphatic involvement [0=no, 1=yes]
#' 14. BULK: bulk involvement >7.5cm (>5.0cm for STUDIDn=999) [0=no, 1=yes]
#' 15. BSYMP: B symptoms [0=no, 1=yes]
#' 16. BM: bone marrow involvement [0=no, 1=yes]
#' 17. REFPATHO: reference pathology TODO
#' 18. EFS: event-free survival in months
#' 19. EFS_STAT: event-free survival status [0=censored, 1=event]
#' 20. PFS: progression-free survival in months
#' 21. PFS_STAT: progression-free survival status [0=censored, 1=event]
#' 22. OS: overall survival in months
#' 23. OS_STAT: overall survival status [0=censored 1=death from any cause]
#' ```
#'
#' Columns 24-20524 contain log2 transformed gene expression values.
#' @examples
#' imlui::lamis_test2[1:3, 1:25]
#' #                STUDIDn PATSTUID ARMID DOSELEVEL GENDER AGE IPI_AGE
#' # Ricover60.S99        7       99     4        NA      1  64       1
#' # Ricover60.S101       7      101     3        NA      0  67       1
#' # Ricover60.S102       7      102     3        NA      0  73       1
#' #                IPI_LDH IPI_ECOG IPI_STAGE IPI_EXBM IPI AA_IPI EXBM BULK
#' # Ricover60.S99        0        0         1        1   3     NA    1    0
#' # Ricover60.S101       1        0         1        0   3     NA    0    1
#' # Ricover60.S102       0        0         0        0   1     NA    1    0
#' #                BSYMP BM REFPATHO      EFS EFS_STAT      PFS PFS_STAT
#' # Ricover60.S99      0  1     1040 109.9630        0 109.9630        0
#' # Ricover60.S101     1  0     1010   4.5339        1   4.5339        1
#' # Ricover60.S102     0  0     1040  54.5051        1  54.5051        1
#' #                      OS OS_STAT    BCL2A1
#' # Ricover60.S99  109.9630       0  9.601771
#' # Ricover60.S101  10.5133       1 10.251482
#' # Ricover60.S102  54.5051       1  7.714246
#' @source
#' \url{https://www.nature.com/articles/s41375-019-0573-y/}
"lamis_test2"
