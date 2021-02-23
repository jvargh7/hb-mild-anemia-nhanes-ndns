#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NHANES 2007-2008
# Owner               : Jithin
# Created date        : 16/01/18
# Modified by         : 
# Last Modified Date  : 16/01/18
#
# Modules :
#     1. Reading and cleaning NHANES 2007-2008 data for Whites
#----------------------------------------------------------------------------
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/CBC_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DEMO_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/HSQ_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/HUQ_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/FERTIN_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/TFR_E.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BIOPRO_E.htm

# 1. Reading and cleaning NHANES 2007-2008 data ----------------

path_data <- "D:/St John's National Academy of Health Sciences/Data Science - 22_Evaluating Hb cutoffs/data"

cbc <- sasxport.get(paste0(path_data,"/nhanes/CBC_E.XPT"),allow=FALSE,as.is=TRUE)
demo <- sasxport.get(paste0(path_data,"/nhanes/DEMO_E.XPT"),allow=FALSE,as.is=TRUE)
hsq <- sasxport.get(paste0(path_data,"/nhanes/HSQ_E.XPT"),allow=FALSE,as.is=TRUE)
huq <- sasxport.get(paste0(path_data,"/nhanes/HUQ_E.XPT"),allow=FALSE,as.is=TRUE)
fertin <- sasxport.get(paste0(path_data,"/nhanes/FERTIN_E.XPT"),allow=FALSE,as.is=TRUE)
tfr <- sasxport.get(paste0(path_data,"/nhanes/TFR_E.XPT"),allow=FALSE,as.is=TRUE)
biopro <- sasxport.get(paste0(path_data,"/nhanes/BIOPRO_E.XPT"),allow=FALSE,as.is=TRUE)
crp <- sasxport.get(paste0(path_data,"/nhanes/CRP_E.XPT"),allow=FALSE,as.is=TRUE)
folate <- sasxport.get(paste0(path_data,"/nhanes/FOLATE_E.XPT"),allow=FALSE,as.is=TRUE)
# sstfr <- sasxport.get(paste0(path_data,"/nhanes/SSTFR_B.XPT"),allow=FALSE,as.is=TRUE)

df <- merge(cbc,demo,by="seqn")
df <- merge(df,hsq,by="seqn")
df <- merge(df,huq,by="seqn")
df <- merge(df,fertin,by="seqn")
df <- merge(df,tfr,by="seqn")
df <- merge(df,biopro,by="seqn")
df <- merge(df,crp,by="seqn")
df <- merge(df,folate,by="seqn")

df[] <- lapply(df, unclass)

df$aim <- with(df,ifelse(is.na(ridagemn),ridageyr*12,ridagemn))
View(df[,c("aim","ridagemn","ridageyr")])
df$who_cat <- with(df,
                   ifelse(aim<6,"C0",
                          ifelse(aim<=59,"C1",
                                 ifelse(aim<12*12,"C2",
                                        ifelse(aim<15*12,"AG1",
                                               ifelse(riagendr==1,"M",
                                                      ifelse(is.na(ridexprg),
                                                             ifelse(aim<240,"U20","A45"),
                                                             ifelse(ridexprg==1,"PW","N+DK"))))))))
View(df[,c("who_cat","ridexprg")])
df <- df[!is.na(df$lbxhgb),]

df <- df[df$who_cat=="N+DK"|df$who_cat=="U20",]
df <- df[df$aim<=(50*12-1),]
df$who_cat2 <- with(df,ifelse(who_cat=="U20","Female- under 20","Female- Non Pregnant and Status Unknown"))

df$ethnicity <- with(df,ifelse(ridreth1 %in% c(1,2),"mexican and hispanic",
                               ifelse(ridreth1==3,"white",ifelse(ridreth1==4,"black","other"))))
# "lbxtib","lbdtibsi",
var_nhanes <- c("seqn","aim","riagendr","ridexprg","who_cat",
                "lbxhgb","huq010",
                "lbxsir","lbdsirsi",
                "lbxtfr",
                "lbxfer","lbdfersi",
                "ethnicity",
                "lbdrbf","lbxrbfsi",
                "lbdfol","lbxfolsi",
                "lbxcrp",
                "lbxmcvsi")
# "tibc_ug","tibc_umol",
col_nhanes <- c("seqn","aim","gender","pregnancy_status","who_cat",
                "hb","health_status",
                "iron_ug","iron_umol",
                "transferrin_receptor_mg_per_l",
                "ferritin_ng_per_ml","ferritin_ug_per_l",
                "ethnicity",
                "folate_rbc_ug", "folate_umol",
                "folate_ser_ug", "folate_ser_umol",
                "crp",
                "mcv")

col_nhanes[!var_nhanes %in% colnames(df)]

#Filtering for ethnicity
df <- df[,var_nhanes]
colnames(df) <- col_nhanes[var_nhanes %in% colnames(df)]
write.xlsx(as.data.frame(df),
           file=paste0(path_data , "/working/nhanes_consolidated_",Sys.Date(),".xlsx"),
           sheetName = "2007-2008",append=TRUE,row.names=FALSE)

saveRDS(df,file=paste0(path_data , "/working/nhanes_2007to2008_v2.RDS"))
