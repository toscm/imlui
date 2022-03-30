# ## Included Datasets
#
# SYMBOL                    SAM   FEAT  TRA  TYP   PRODUCER                 PLATFORM      PAPER
# dshnhl.nanostring.rccset  466    159  yes  RNA   TODO                     NSNC          [LAMIS-2019]
# dshnhl.pepFull.eset       360   9070  yes  Pep   Oefner Lab (Kathi Limm)  SWATH-MS      TODO
# dshnhl.pheno              466     52  no   Phe   DSHNHL studies[1]        Excel         [LAMIS-2019]
# dshnhl.proteins_mat       360   2890  yes  Pro   Oefner Lab (Kathi Limm)  SWATH-MS      TODO
# ghsg.set                  401    156  yes  RNA   TODO                     TODO          *BEACOPP-2021*
# lamis_train               233  20524  no   RNA   TODO                     Affy MA       [LAMIS-2019]
# lamis_test1               233  20524  no   RNA   TODO                     Affy MA       [LAMIS-2019]
# lamis_test2               466    159  no   RNA   TODO                     NSNC          [LAMIS-2019]
# miR_Ros.combined          258    822  yes  RNA   Rosenwald Lab            NSNC          [MicroRNA-2021]
# miR_Ros.rep.v2.1           24    822  yes  RNA   Rosenwald Lab            NSNC v2.1     [MicroRNA-2021]
# miR_Ros.rep.v2             24    822  yes  RNA   Rosenwald Lab            NSNC v2.0     [MicroRNA-2021]
# miR_Ros.v2.1              151    822  yes  RNA   Rosenwald Lab            NSNC v2.1     [MicroRNA-2021]
# miR_Ros.v2                107    822  yes  RNA   Rosenwald Lab            NSNC v2.0     [MicroRNA-2021]
# mpi.microarray.mat        938  12403  yes  RNA   TODO                     Affy MA       TODO
# mpi.pheno                 935    166  no   Phe   TODO                     Excel         TODO
# norm.jco.test              78    266  yes  RNA   TODO                     TODO          [JCO-2013]
# norm.jco.train            290    266  yes  RNA   TODO                     TODO          [JCO-2013]
# rccSet_labvgl              48    159  yes  RNA   TODO                     TODO          [COO-2017]? (TODO)
# rccSet_Richter            360    131  yes  RNA   TODO                     TODO          [COO-2017]? (TODO)
# rccSet.petal              360    131  yes  RNA   TODO                     TODO          [PETAL-2018]
# wolframs_mouse_data        28   1960  no   Prot  Oefner Lab (TODO)        TODO          TODO
#
# ## Included signatures
#
# * modified.signature_jco
# * sig.coo.cutoffs
# * sig.coo
# * signature.JCO
# * sig.LAMIS.ratio
# * sig.LAMIS.single

PKG <- c(
    # dshnhl.nanostring.rccset = "DataLoading.lfs",
    # dshnhl.pepFull.eset = "DataLoading.lfs",
    # dshnhl.pheno = "DataLoading.lfs",
    # dshnhl.proteins_mat = "DataLoading.lfs",
    # ghsg.set = "DataLoading.lfs",
    # miR_Ros.combined = "DataLoading.lfs",
    # miR_Ros.rep.v2 = "DataLoading.lfs",
    # miR_Ros.rep.v2.1 = "DataLoading.lfs",
    # miR_Ros.v2 = "DataLoading.lfs",
    # miR_Ros.v2.1 = "DataLoading.lfs",
    # modified.signature_jco = "DataLoading.lfs",
    # mpi.microarray.mat = "DataLoading.lfs",
    # mpi.pheno = "DataLoading.lfs",
    # norm.jco.test = "DataLoading.lfs",
    # norm.jco.train = "DataLoading.lfs",
    # rccSet_labvgl = "DataLoading.lfs",
    # rccSet_Richter = "DataLoading.lfs",
    # rccSet.petal = "DataLoading.lfs",
    # sig.coo = "DataLoading.lfs",
    # sig.coo.cutoffs = "DataLoading.lfs",
    # sig.LAMIS.ratio = "DataLoading.lfs",
    # sig.LAMIS.single = "DataLoading.lfs",
    # signature.JCO = "DataLoading.lfs",
    # wolframs_mouse_data = "imlui",
    lamis_signature = "toscdata",
    lamis_test1 = "toscdata",
    lamis_test2 = "toscdata",
    lamis_train = "toscdata",
    nordmo_os_signature = "toscdata",
    nordmo_pfs_signature = "toscdata",
    nordmo_train = "toscdata",
    reinders_coo_signature = "toscdata",
    seifert_coo_signature = "toscdata",
    seifert_test = "toscdata",
    seifert_train = "toscdata",
    seifert_tric_signature = "toscdata"
)

datasets <- c(
    "lamis_test1",
    "lamis_test2",
    "lamis_train",
    "nordmo_train",
    "seifert_test",
    "seifert_train"
    # "dshnhl.nanostring.rccset",
    # "dshnhl.pepFull.eset",
    # "dshnhl.pheno",
    # "dshnhl.proteins_mat",
    # "ghsg.set",
    # "miR_Ros.combined",
    # "miR_Ros.rep.v2.1",
    # "miR_Ros.rep.v2",
    # "miR_Ros.v2.1",
    # "miR_Ros.v2",
    # "mpi.microarray.mat",
    # "mpi.pheno",
    # "norm.jco.test",
    # "norm.jco.train",
    # "rccSet_labvgl",
    # "rccSet_Richter",
    # "rccSet.petal",
    # "wolframs_mouse_data"
)

TRANSPOSE_REQUIRED <- c(
    seifert_train=FALSE,
    seifert_test=FALSE,
    nordmo_train=FALSE,
    lamis_train=FALSE,
    lamis_test2=FALSE,
    lamis_test1=FALSE
    # dshnhl.nanostring.rccset = TRUE,
    # dshnhl.pepFull.eset = TRUE,
    # dshnhl.pheno = FALSE,
    # dshnhl.proteins_mat = TRUE,
    # ghsg.set = TRUE,
    # lamis_train = FALSE,
    # lamis_test1 = FALSE,
    # lamis_test2 = FALSE,
    # miR_Ros.combined = TRUE,
    # miR_Ros.rep.v2.1 = TRUE,
    # miR_Ros.rep.v2 = TRUE,
    # miR_Ros.v2.1 = TRUE,
    # miR_Ros.v2 = TRUE,
    # mpi.microarray.mat = TRUE,
    # mpi.pheno = FALSE,
    # norm.jco.test = TRUE,
    # norm.jco.train = TRUE,
    # rccSet_labvgl = TRUE,
    # rccSet_Richter = TRUE,
    # rccSet.petal = TRUE,
    # wolframs_mouse_data = FALSE
)

# Format: `newName` = "oldNname"
FEATURE_MAPPINGS <- list(
    norm.jco.train = c(
        `HLA-A` = "HLA.A",
        `HLA-C` = "HLA.C"
    ),
    norm.jco.test = c(
        `HLA-A` = "HLA.A",
        `HLA-C` = "HLA.C"
    )
)

CATEGORICALS <- c(
    "STUDIDn", "PATSTUDID", "Individual", "PATIENTEN_ID"
)

metamodels <- structure(
    list(y_name = c("Lamis Scores")),
    class = "data.frame",
    row.names = c("lamis_signature")
)

metadata <- structure(
    list(
        package =      c("imlui", "imlui", "imlui"),
        batch_col =    c("", "", "STUDIDn"),
        sample_col =   c("Individual", "Individual", "PATSTUID"),
        os_col =       c("Follow.up.years", "Follow.up.years", "OS"),
        os_unit =      c("years", "years", "months"),
        os_stat_col =  c("Follow.up.status", "Follow.up.status", "OS_STAT"),
        efs_col =      c("", "", "EFS"),
        efs_unit =     c("", "", "months"),
        efs_stat_col = c("", "", "EFS_STAT"),
        pfs_col =      c("", "", "PFS"),
        pfs_unit =     c("", "", "months"),
        pfs_stat_col = c("", "", "PFS_STAT")
    ),
    class = "data.frame",
    row.names = c("lamis_train", "lamis_test1", "lamis_test2")
)