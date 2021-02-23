#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NDNS 2008-2012
# Owner               : Jithin
# Created date        : 16/01/18
# Modified by         : 
# Last Modified Date  : 16/01/18
#
# Modules :
#     1. Reading and cleaning NDNS 2008-2012 data for Whites
#----------------------------------------------------------------------------

path_data <- "D:/St John's National Academy of Health Sciences/Data Science - 22_Evaluating Hb cutoffs/data"

df <- read_dta(paste0(path_data,"/ndns/ndns_rp_yr1-4a_indiv_core.dta"))

df <- df[df$Sex==2,]
#20 to 44
df <- df[df$age>=15 & df$age<=49,]
df <- df[df$Hb!=-1,]

df$ethnicity <- "other"
df$ethnicity <- with(df,ifelse(EthGrG %in% c(1,2) | 
                                 EthGrN %in% c(1,2) | 
                                 EthGrU %in% c(1),"white",
                               ifelse(EthGrG %in% c(7,8,9) | 
                                        EthGrN %in% c(4,5,6) | 
                                        EthGrU %in% c(3,4,5),"south asian",
                                      ifelse(EthGrG %in% c(11,12,13) | 
                                               EthGrN %in% c(8,9,10) | 
                                               EthGrU %in% c(7,8,9),"black",
                                             ifelse(EthGrG %in% c(14) | 
                                                      EthGrN %in% c(11) | 
                                                      EthGrU %in% c(10),"chinese",
                                                    ifelse(EthGrG %in% c(10) | 
                                                             EthGrN %in% c(7) | 
                                                             EthGrU %in% c(6),"other asian","other"))))))

df$seqn <- seq(1:nrow(df))
df$aim <- df$age*12
df$pregnancy_status <- with(df,ifelse(PregNTJ==1 | PregNowB == 1,1,2))
df$who_cat <- with(df,ifelse(pregnancy_status==1,"PW","N+DK"))

var_ndns_y1to4 <- c("seqn","aim","Sex","pregnancy_status","who_cat",
                    "Hb","GenHelf",
                    "iron","PFerritin","PFerritinRes","sTFR","sTFRRes","ethnicity",
                    "CRP", "CRPRes", "SVitB12", "SVitB12Res", "MCV", "MCVRes")

col_ndns_y1to4 <- c("seqn","aim","gender","pregnancy_status","who_cat",
                "hb","health_status",
                "iron_deficiency_tx","ferritin_ug_per_l","ferritin_result",
                "soluble_transferrin_receptor","stfr_result",
                "ethnicity",
                "CRP", "CRPRes", "vitamin_B12", "vitamin_B12_Res", "MCV", "MCVRes")
col_ndns_y1to4[!var_ndns_y1to4 %in% colnames(df)]

df <- df[,var_ndns_y1to4]
colnames(df) <- col_ndns_y1to4[var_ndns_y1to4 %in% colnames(df)]

saveRDS(df, paste0(path_data,"/working/ndns_y1to4_2008-2012_v2.RDS"))
write.xlsx(as.data.frame(df),
           file=paste0(path_data , "/working/ndns_consolidated_",Sys.Date(),".xlsx"),
           sheetName = "2008-2012",append=TRUE,row.names=FALSE)

