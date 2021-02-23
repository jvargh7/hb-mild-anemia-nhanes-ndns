#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NDNS 2000-2001
# Owner               : Jithin
# Created date        : 16/01/18
# Modified by         : 
# Last Modified Date  : 16/01/18
#
# Modules :
#     1. Reading and cleaning NDNS 2000-2001 data for Whites
#----------------------------------------------------------------------------

path_data <- "D:/St John's National Academy of Health Sciences/Data Science - 22_Evaluating Hb cutoffs/data"

bloodanalytes <- read_dta(paste0(path_data,"/ndns/ndns_19to64_bloodanalytes.dta"))
interview <- read_dta(paste0(path_data,"/ndns/ndns_19to64_interview.dta"))

df <- merge(bloodanalytes,interview[,c("caseid","preg")],by="caseid")
df[] <- lapply(df, unclass)
df <- df[df$respsex==2,]

df <- df[df$respage>19 & df$respage<=49,]

df <- df[df$hb>0,]

df$who_cat <- with(df,ifelse(preg==2,"N+DK","PW"))

df <- df[df$who_cat =="N+DK",]

df$ethnicity <- with(df,ifelse(ethnic==1,"white",
                               ifelse(ethnic %in% c(2,3,4),"black",
                                      ifelse(ethnic %in% c(5,6,7),"south asian",
                                             ifelse(ethnic %in% c(8),"chinese","other")))))
df$aim <- df$respage*12
var_ndns_19to64 <- c("caseid","aim","respsex","preg","who_cat","hb"
                     , "pfe","psat"
                     , "ptibc"
                     , "gpfe","gpsat"
                     , "gptibc"
                     , "ethnicity"
                     , "sferr",  "adjsferr"
                     , "sb12"
                     , "rcfol", "adjrcfol"
                     , "sfol", "adjsfol"
                     , "mcv", "adjmcv")

col_ndns_19to64 <- c("seqn", "aim", "gender", "pregnancy_status", "who_cat", "hb"
                     , "iron_umol", "iron_saturation"
                     , "tibc_umol"
                     , "grouped_iron_umol", "grouped_iron_saturation"
                     , "grouped_tibc_umol"
                     , "ethnicity"
                     , "ferritin", "ferritin_adj"
                     , "Vit_B12"
                     , "folate_rbc", "folate_rbc_adj"
                     , "folate_ser", "folate_ser_adj"
                     , "mcv", "adjmcv")

col_ndns_19to64[!var_ndns_19to64 %in% colnames(df)]
df <- df[,var_ndns_19to64]
colnames(df) <- col_ndns_19to64[var_ndns_19to64 %in% colnames(df)]

saveRDS(df, paste0(path_data,"/working/ndns_19to64_2000-2001_v1.RDS"))
write.xlsx(as.data.frame(df),
           file=paste0(path_data , "/working/ndns_consolidated_",Sys.Date(),".xlsx"),
           sheetName = "2000-2001",append=TRUE,row.names=FALSE)

