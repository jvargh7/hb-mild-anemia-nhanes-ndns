#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NHANES 2003-2004
# Owner               : Jithin
# Created date        : 16/01/18
# Modified by         : 
# Last Modified Date  : 16/01/18
#
# Modules :
#     1. Reading and cleaning NHANES 2003-2004 data for Whites
#----------------------------------------------------------------------------
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L25_C.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/HSQ_C.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/HUQ_C.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L06TFR_C.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L40FE_C.htm

# 1. Reading and cleaning NHANES 1999-2000 data ----------------

path_data <- "D:/St John's National Academy of Health Sciences/Data Science - 22_Evaluating Hb cutoffs/data"

cbc <- sasxport.get(paste0(path_data,"/nhanes/CBC_C.XPT"),allow=FALSE,as.is=TRUE)
demo <- sasxport.get(paste0(path_data,"/nhanes/DEMO_C.XPT"),allow=FALSE,as.is=TRUE)
hsq <- sasxport.get(paste0(path_data,"/nhanes/HSQ_C.XPT"),allow=FALSE,as.is=TRUE)
huq <- sasxport.get(paste0(path_data,"/nhanes/HUQ_C.XPT"),allow=FALSE,as.is=TRUE)
l06 <- sasxport.get(paste0(path_data,"/nhanes/L06_C.XPT"),allow=FALSE,as.is=TRUE)
l06nb <- sasxport.get(paste0(path_data,"/nhanes/L06NB_C.XPT"),allow=FALSE,as.is=TRUE)
l40fe <- sasxport.get(paste0(path_data,"/nhanes/L40FE_C.XPT"),allow=FALSE,as.is=TRUE)
l11 <- sasxport.get(paste0(path_data,"/nhanes/L11_C.XPT"),allow=FALSE,as.is=TRUE)
# sstfr <- sasxport.get(paste0(path_data,"/nhanes/SSTFR_B.XPT"),allow=FALSE,as.is=TRUE)

df <- merge(cbc,demo,by="seqn")
df <- merge(df,hsq,by="seqn")
df <- merge(df,huq,by="seqn")
df <- merge(df,l06,by="seqn")
df <- merge(df,l06nb,by="seqn")
df <- merge(df,l40fe,by="seqn")
df <- merge(df,l11,by="seqn")
# df <- merge(df,sstfr,by="seqn")

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

var_nhanes <- c("seqn","aim","riagendr","ridexprg","who_cat",
                "lbxhgb","huq010",
                "lbxirn","lbdirnsi",
                "lbxtib","lbdtibsi",
                "lbdpct",
                "lbdfer","lbdfersi",
                "ethnicity",
                "lbxrbf","lbdrbfsi",
                "lbxfol","lbdfolsi",
                "lbxb12" , "lbdb12si",
                "lbxcrp",
                "lbxmcvsi")
col_nhanes <- c("seqn","aim","gender","pregnancy_status","who_cat",
                "hb","health_status",
                "iron_ug","iron_umol",
                "tibc_ug","tibc_umol",
                "transferrin_saturation",
                "ferritin_ng_per_ml","ferritin_ug_per_l",
                "ethnicity",
                "folate_rbc_ug", "folate_umol",
                "folate_ser_ug", "folate_ser_umol",
                "vit_b12_ug", "vit_b12_ug_umol",
                "crp",
                "mcv")

col_nhanes[!var_nhanes %in% colnames(df)]
#Filtering for ethnicity
df <- df[,var_nhanes]
colnames(df) <- col_nhanes[var_nhanes %in% colnames(df)]
write.xlsx(as.data.frame(df),
           file=paste0(path_data , "/working/nhanes_consolidated_",Sys.Date(),".xlsx"),
           sheetName = "2003-2004",append=TRUE,row.names=FALSE)

saveRDS(df,file=paste0(path_data , "/working/nhanes_2003to2004_v2.RDS"))

