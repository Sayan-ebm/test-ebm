#========================================================
# DataLoad.R
# Load all default datasets
#========================================================
#----------------------------
# Comparator OS IPD
#----------------------------
default_bgm <- read.csv("data/background_mortality.csv")
#----------------------------
# Comparator OS IPD
#----------------------------
default_os_ipd <- read.csv("data/enza_os_pipd.csv")
#----------------------------
# Comparator PFS IPD
#----------------------------
default_pfs_ipd <- read.csv("data/enza_pfs_pipd.csv")
#----------------------------
# Intervention OS IPD
#----------------------------
default_os_ipd_intervention <- read.csv("data/enza_radium_os_pipd.csv")
#----------------------------
# Intervention PFS IPD
#----------------------------
default_pfs_ipd_intervention <- read.csv("data/enza_radium_pfs_pipd.csv")
#========================================================
# COMPARATOR DIGITIZED KM
#========================================================
default_os_survival <- read.csv("data/enza_prob_os.csv")
default_os_risk <- read.csv("data/enza_natrisk_os.csv")
#----------------------------
# PFS
#----------------------------
default_pfs_survival <- read.csv("data/enza_prob_pfs.csv")
default_pfs_risk <- read.csv("data/enza_natrisk_pfs.csv")
#----------------------------
# OS
#----------------------------
default_os_survival_intervention <- read.csv("data/enza_rad_prob_os.csv")
default_os_risk_intervention <- read.csv("data/enza_rad_natrisk_os.csv")
#----------------------------
# PFS
#----------------------------
default_pfs_survival_intervention <- read.csv("data/enza_rad_prob_pfs.csv")
default_pfs_risk_intervention <- read.csv("data/enza_rad_natrisk_pfs.csv")
#========================================================
# ADVERSE EVENTS
#========================================================
default_ae_probabilities <- read.csv("data/ae_probabilities.csv")
#========================================================
# Long term validation files
#========================================================
longterm_os_comp <- read.csv("data/os_ballal.csv")
lonterm_pfs_comp <- read.csv("data/pfs_ballal.csv")

longterm_os_int <- read.csv("data/os_ballal.csv")
longterm_pfs_int <- read.csv("data/pfs_ballal.csv")


#========================================================
# DATABASE CONNECTION
#========================================================
mydb <- DBI::dbConnect(RSQLite::SQLite(),":memory:")
#========================================================
# STORE TABLES
#========================================================
DBI::dbWriteTable(mydb, "surv_est_data_OS", default_os_ipd, overwrite = TRUE)
DBI::dbWriteTable(mydb, "surv_est_data_PFS", default_pfs_ipd, overwrite = TRUE)
DBI::dbWriteTable(mydb, "surv_est_data_OS_intervention", 
                  default_os_ipd_intervention, overwrite = TRUE)
DBI::dbWriteTable(mydb, "surv_est_data_PFS_intervention", 
                  default_pfs_ipd_intervention, overwrite = TRUE)
DBI::dbWriteTable(mydb, "AE", default_ae_probabilities, overwrite = TRUE)
#========================================================
# RETRIEVE AE TABLES
#========================================================
ae_probabilities <- DBI::dbGetQuery(mydb, "SELECT * FROM AE")

