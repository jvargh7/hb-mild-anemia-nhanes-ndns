#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NHANES and NDNS data
# Owner               : Jithin
# Created date        : 15/01/18
# Modified by         : 
# Last Modified Date  : 15/01/18
#
# Modules :
#     1. Reading and cleaning NHANES data
#     2. Reading and cleaning NDNS Y1to4
#     3. Reading and cleaning NDNS 19to64 blood analytes
#----------------------------------------------------------------------------


# 1. Reading and cleaning NHANES data -------------------------------------

path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

cbc_h <- sasxport.get(paste0(path_data,"/nhanes_2013-14/CBC_H.XPT"),allow=FALSE,as.is=TRUE)
demo_h <- sasxport.get(paste0(path_data,"/nhanes_2013-14/DEMO_H.XPT"),allow=FALSE,as.is=TRUE)

cbc_demo <- merge(cbc_h,demo_h,by.x="seqn",by.y="seqn")

cbc_demo[] <- lapply(cbc_demo, unclass)

cbc_demo$aim <- with(cbc_demo,ifelse(is.na(ridagemn),ridageyr*12,ridagemn))
cbc_demo$who_cat <- with(cbc_demo,
                         ifelse(aim<6,"C0",
                                ifelse(aim<=59,"C1",
                                       ifelse(aim<12*12,"C2",
                                              ifelse(aim<15*12,"AG1",
                                                     ifelse(riagendr==1,"M",
                                                            ifelse(is.na(ridexprg),
                                                                   ifelse(aim<240,"U20","A45"),
                                                                   ifelse(ridexprg==1,"PW","N+DK"))))))))
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm
#Ethinicity: All Non-Hispanic Asian
#who_cat includes Females between 15 to 20 who could be pregnant as U20
cbc_demo_hb <- cbc_demo[!is.na(cbc_demo$lbxhgb),]
non_hisp_asian <- cbc_demo_hb[cbc_demo_hb$ridreth3==6,]
non_hisp_asian_np <- non_hisp_asian[non_hisp_asian$who_cat=="N+DK"|non_hisp_asian$who_cat=="U20",]
non_hisp_asian_np <- non_hisp_asian_np[non_hisp_asian_np$aim<=45*12,]
non_hisp_asian_np$who_cat2 <- with(non_hisp_asian_np,ifelse(who_cat=="U20","Female- under 20","Female- Non Pregnant and Status Unknown"))

non_hisp_asian_np <- non_hisp_asian_np %>% group_by(who_cat) %>% mutate(median_nhanes = median(lbxhgb),
                                                                        mean_nhanes = mean(lbxhgb))
non_hisp_asian_np <- non_hisp_asian_np %>% group_by(who_cat) %>% mutate(sd_nhanes = sd(lbxhgb))

var_nhanes <- c("seqn","aim","riagendr","ridexprg","ridreth3","who_cat",
                "lbxhgb","median_nhanes","mean_nhanes","sd_nhanes")

non_hisp_asian_np <- non_hisp_asian_np[,var_nhanes]
saveRDS(non_hisp_asian_np,paste0(path_data,"/working/nhanes2013-14_non_hisp_asian_np.RDS"))



# 2. Reading and cleaning NDNS Y1to4 --------------------------------------
path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

ndns_y1to4_individual <- read_dta(paste0(path_data,"/ndns/ndns_rp_yr1-4a_indiv_core.dta"))
ndns_y1to4_individual_sa <- ndns_y1to4_individual[ndns_y1to4_individual$EthGrG %in% c(7,8,9)|
                                       ndns_y1to4_individual$EthGrN %in% c(4,5,6)|
                                       ndns_y1to4_individual$EthGrU %in% c(3,4,5),]
ndns_y1to4_individual_sa_f <- ndns_y1to4_individual_sa[ndns_y1to4_individual_sa$Sex==2,]
#20 to 44
ndns_y1to4_individual_sa_f_a20u45 <- ndns_y1to4_individual_sa_f[ndns_y1to4_individual_sa_f$age>=20&
                                                                  ndns_y1to4_individual_sa_f$age<=44,]
ndns_y1to4_individual_sa_f_a20u45_hb <- 
  ndns_y1to4_individual_sa_f_a20u45[ndns_y1to4_individual_sa_f_a20u45$Hb!=-1,]

var_ndns_y1to4 <- c("EthGrG","EthGrN","EthGrU","Sex",
                    "age","Hb",
                    "PregNTJ","UPreg","PregNowB",
                    "iron","PFerritin","PFerritinRes","sTFR","sTFRRes")

ndns_y1to4_individual_sa_f_a20u45_hb <- ndns_y1to4_individual_sa_f_a20u45_hb[,var_ndns_y1to4]
ndns_y1to4_individual_sa_f_a20u45_hb[] <- lapply(ndns_y1to4_individual_sa_f_a20u45_hb, unclass)
saveRDS(ndns_y1to4_individual_sa_f_a20u45_hb,
        paste0(path_data,"/working/ndns_y1to4_individual_sa_f_a20u45_hb.RDS"))


# 3. Reading and cleaning NDNS 19to64 blood analytes ----------------------
path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

ndns_19to64_bloodanalytes <- read_dta(paste0(path_data,"/ndns/ndns_19to64_bloodanalytes.dta"))

ndns_19to64_bloodanalytes_sa <- ndns_19to64_bloodanalytes[ndns_19to64_bloodanalytes$ethnic %in% c(5,6,7),]

ndns_19to64_bloodanalytes_sa_f <- ndns_19to64_bloodanalytes_sa[ndns_19to64_bloodanalytes_sa$respsex==2,]

ndns_19to64_bloodanalytes_sa_f_a20u45 <- 
  ndns_19to64_bloodanalytes_sa_f[ndns_19to64_bloodanalytes_sa_f$respage>19 & 
                                   ndns_19to64_bloodanalytes_sa_f$respage<45,]

ndns_19to64_bloodanalytes_sa_f_a20u45_hb <- 
  ndns_19to64_bloodanalytes_sa_f_a20u45[ndns_19to64_bloodanalytes_sa_f_a20u45$hb>0,]

#Merging with interview.dta for pregnancy status
ndns_19to64_interview <- read_dta(paste0(path_data,"/ndns/ndns_19to64_interview.dta"))
ndns_19to64_bloodanalytes_sa_f_a20u45_hb <- merge(ndns_19to64_bloodanalytes_sa_f_a20u45_hb,
                                                  ndns_19to64_interview[,c("caseid","preg")],by="caseid")
ndns_19to64_bloodanalytes_sa_f_a20u45_hb[] <- lapply(ndns_19to64_bloodanalytes_sa_f_a20u45_hb, unclass)

ndns_19to64_bloodanalytes_sa_f_a20u45_hb_np <- 
  ndns_19to64_bloodanalytes_sa_f_a20u45_hb[ndns_19to64_bloodanalytes_sa_f_a20u45_hb$preg==2,]

var_ndns_19to64 <- c("caseid","respsex","respage","preg",
                     "hb","ethnic",
                     "pfe","ptibc","psat","gpfe","gptibc","gpsat")

ndns_19to64_bloodanalytes_sa_f_a20u45_hb_np <- ndns_19to64_bloodanalytes_sa_f_a20u45_hb[,var_ndns_19to64]
saveRDS(ndns_19to64_bloodanalytes_sa_f_a20u45_hb_np,
        paste0(path_data,"/working/ndns_19to64_bloodanalytes_sa_f_a20u45_hb_np.RDS"))



# 4. Reading and cleaning Caucasian ethnicity- NHANES -----------------------------

path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

cbc_h <- sasxport.get(paste0(path_data,"/nhanes_2013-14/CBC_H.XPT"),allow=FALSE,as.is=TRUE)
demo_h <- sasxport.get(paste0(path_data,"/nhanes_2013-14/DEMO_H.XPT"),allow=FALSE,as.is=TRUE)

cbc_demo <- merge(cbc_h,demo_h,by.x="seqn",by.y="seqn")

cbc_demo[] <- lapply(cbc_demo, unclass)

cbc_demo$aim <- with(cbc_demo,ifelse(is.na(ridagemn),ridageyr*12,ridagemn))
cbc_demo$who_cat <- with(cbc_demo,
                         ifelse(aim<6,"C0",
                                ifelse(aim<=59,"C1",
                                       ifelse(aim<12*12,"C2",
                                              ifelse(aim<15*12,"AG1",
                                                     ifelse(riagendr==1,"M",
                                                            ifelse(is.na(ridexprg),
                                                                   ifelse(aim<240,"U20","A45"),
                                                                   ifelse(ridexprg==1,"PW","N+DK"))))))))
# https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.htm
#Ethinicity: All Non-Hispanic Asian
#who_cat includes Females between 15 to 20 who could be pregnant as U20
cbc_demo_hb <- cbc_demo[!is.na(cbc_demo$lbxhgb),]
non_hisp_white <- cbc_demo_hb[cbc_demo_hb$ridreth3==3,]
non_hisp_white_np <- non_hisp_white[non_hisp_white$who_cat=="N+DK"|non_hisp_white$who_cat=="U20",]
non_hisp_white_np <- non_hisp_white_np[non_hisp_white_np$aim<=45*12,]
non_hisp_white_np$who_cat2 <- with(non_hisp_white_np,ifelse(who_cat=="U20","Female- under 20","Female- Non Pregnant and Status Unknown"))

non_hisp_white_np <- non_hisp_white_np %>% group_by(who_cat) %>% mutate(median_nhanes = median(lbxhgb),
                                                                        mean_nhanes = mean(lbxhgb))
non_hisp_white_np <- non_hisp_white_np %>% group_by(who_cat) %>% mutate(sd_nhanes = sd(lbxhgb))

var_nhanes <- c("seqn","aim","riagendr","ridexprg","ridreth3","who_cat",
                "lbxhgb","median_nhanes","mean_nhanes","sd_nhanes")

non_hisp_white_np <- non_hisp_white_np[,var_nhanes]
saveRDS(non_hisp_white_np,paste0(path_data,"/working/nhanes2013-14_non_hisp_white_np.RDS"))



# 5. Cleaning for Caucasian- NDNS Y1to4 -----------------------------------

path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

ndns_y1to4_individual <- read_dta(paste0(path_data,"/ndns/ndns_rp_yr1-4a_indiv_core.dta"))
ndns_y1to4_individual_wh <- ndns_y1to4_individual[ndns_y1to4_individual$EthGrG %in% c(1,2)|
                                                    ndns_y1to4_individual$EthGrN %in% c(1,2)|
                                                    ndns_y1to4_individual$EthGrU %in% c(1),]
ndns_y1to4_individual_wh_f <- ndns_y1to4_individual_wh[ndns_y1to4_individual_wh$Sex==2,]
#20 to 44
ndns_y1to4_individual_wh_f_a20u45 <- ndns_y1to4_individual_wh_f[ndns_y1to4_individual_wh_f$age>=20&
                                                                  ndns_y1to4_individual_wh_f$age<=44,]
ndns_y1to4_individual_wh_f_a20u45_hb <- 
  ndns_y1to4_individual_wh_f_a20u45[ndns_y1to4_individual_wh_f_a20u45$Hb!=-1,]

var_ndns_y1to4 <- c("EthGrG","EthGrN","EthGrU","Sex",
                    "age","Hb",
                    "PregNTJ","UPreg","PregNowB",
                    "iron","PFerritin","PFerritinRes","sTFR","sTFRRes")

ndns_y1to4_individual_wh_f_a20u45_hb <- ndns_y1to4_individual_wh_f_a20u45_hb[,var_ndns_y1to4]
ndns_y1to4_individual_wh_f_a20u45_hb[] <- lapply(ndns_y1to4_individual_wh_f_a20u45_hb, unclass)
saveRDS(ndns_y1to4_individual_wh_f_a20u45_hb,
        paste0(path_data,"/working/ndns_y1to4_individual_wh_f_a20u45_hb.RDS"))



# 6. Reading and cleaning NDNS 19to64- Caucasians -------------------------------------

path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

ndns_19to64_bloodanalytes <- read_dta(paste0(path_data,"/ndns/ndns_19to64_bloodanalytes.dta"))

ndns_19to64_bloodanalytes_wh <- ndns_19to64_bloodanalytes[ndns_19to64_bloodanalytes$ethnic %in% c(1),]

ndns_19to64_bloodanalytes_wh_f <- ndns_19to64_bloodanalytes_wh[ndns_19to64_bloodanalytes_wh$respsex==2,]

ndns_19to64_bloodanalytes_wh_f_a20u45 <- 
  ndns_19to64_bloodanalytes_wh_f[ndns_19to64_bloodanalytes_wh_f$respage>19 & 
                                   ndns_19to64_bloodanalytes_wh_f$respage<45,]

ndns_19to64_bloodanalytes_wh_f_a20u45_hb <- 
  ndns_19to64_bloodanalytes_wh_f_a20u45[ndns_19to64_bloodanalytes_wh_f_a20u45$hb>0,]

#Merging with interview.dta for pregnancy status
ndns_19to64_interview <- read_dta(paste0(path_data,"/ndns/ndns_19to64_interview.dta"))
ndns_19to64_bloodanalytes_wh_f_a20u45_hb <- merge(ndns_19to64_bloodanalytes_wh_f_a20u45_hb,
                                                  ndns_19to64_interview[,c("caseid","preg")],by="caseid")
ndns_19to64_bloodanalytes_wh_f_a20u45_hb[] <- lapply(ndns_19to64_bloodanalytes_wh_f_a20u45_hb, unclass)

ndns_19to64_bloodanalytes_wh_f_a20u45_hb_np <- 
  ndns_19to64_bloodanalytes_wh_f_a20u45_hb[ndns_19to64_bloodanalytes_wh_f_a20u45_hb$preg==2,]

var_ndns_19to64 <- c("caseid","respsex","respage","preg",
                     "hb","ethnic",
                     "pfe","ptibc","psat","gpfe","gptibc","gpsat")

ndns_19to64_bloodanalytes_wh_f_a20u45_hb_np <- ndns_19to64_bloodanalytes_wh_f_a20u45_hb[,var_ndns_19to64]
saveRDS(ndns_19to64_bloodanalytes_wh_f_a20u45_hb_np,
        paste0(path_data,"/working/ndns_19to64_bloodanalytes_wh_f_a20u45_hb_np.RDS"))

