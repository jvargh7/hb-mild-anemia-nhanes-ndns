#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NHANES 2011-2012
# Owner               : Jithin
# Created date        : 16/01/18
# Modified by         : 
# Last Modified Date  : 16/01/18
#
# Modules :
#     1. Reading and cleaning NHANES 2011-2012 data for Whites
#----------------------------------------------------------------------------
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/CBC_G.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HSQ_G.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/HUQ_G.htm
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BIOPRO_G.htm

# 1. Reading and cleaning NHANES 2013-2014 data ----------------

path_data <- "D:/St John's National Academy of Health Sciences/Data Science - 22_Evaluating Hb cutoffs/data"

cbc <- sasxport.get(paste0(path_data,"/nhanes/CBC_G.XPT"),allow=FALSE,as.is=TRUE)
demo <- sasxport.get(paste0(path_data,"/nhanes/DEMO_G.XPT"),allow=FALSE,as.is=TRUE)
hsq <- sasxport.get(paste0(path_data,"/nhanes/HSQ_G.XPT"),allow=FALSE,as.is=TRUE)
huq <- sasxport.get(paste0(path_data,"/nhanes/HUQ_G.XPT"),allow=FALSE,as.is=TRUE)
biopro <- sasxport.get(paste0(path_data,"/nhanes/BIOPRO_G.XPT"),allow=FALSE,as.is=TRUE)
b12 <- sasxport.get(paste0(path_data,"/nhanes/VITB12_G.XPT"),allow=FALSE,as.is=TRUE)


df <- merge(cbc,demo,by="seqn")
df <- merge(df,hsq,by="seqn")
df <- merge(df,huq,by="seqn")
df <- merge(df,biopro,by="seqn")
df <- merge(df,b12,by="seqn")

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

df$ethnicity <- with(df,ifelse(ridreth3 %in% c(1,2),"mexican and hispanic",
                               ifelse(ridreth3==3,"white",ifelse(ridreth3==4,"black",
                                                                 ifelse(ridreth3==6,"asian","other")))))
# "lbxtib","lbdtibsi",
var_nhanes <- c("seqn","aim","riagendr","ridexprg","who_cat",
                "lbxhgb","huq010",
                "lbxsir","lbdsirsi",
                "ethnicity",
                "lbxmcvsi")
# "tibc_ug","tibc_umol",
col_nhanes <- c("seqn","aim","gender","pregnancy_status","who_cat",
                "hb","health_status",
                "iron_ug","iron_umol",
                "ethnicity",
                "mcv")

col_nhanes[!var_nhanes %in% colnames(df)]

#Filtering for ethnicity
df <- df[,var_nhanes]
colnames(df) <- col_nhanes[var_nhanes %in% colnames(df)]
write.xlsx(as.data.frame(df),
           file=paste0(path_data , "/working/nhanes_consolidated_",Sys.Date(),".xlsx"),
           sheetName = "2011-2012",append=TRUE,row.names=FALSE)

saveRDS(df,file=paste0(path_data , "/working/nhanes_2011to2012_v2.RDS"))
