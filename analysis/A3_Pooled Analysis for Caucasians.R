#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Pooled analysis of Hb cutoffs for Caucasians
# Owner               : Jithin
# Created date        : 15/01/18
# Modified by         : 
# Last Modified Date  : 15/01/18
#
# Modules :
#     1. Appending different datasets
#     2. Pooled histogram
#----------------------------------------------------------------------------


# 1. Appending different datasets -----------------------------------------
path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

nhanes_2013 <- readRDS(paste0(path_data,"/working/nhanes2013-14_non_hisp_white_np.RDS"))
ndns_y1to4 <- readRDS(paste0(path_data,"/working/ndns_y1to4_individual_wh_f_a20u45_hb.RDS"))
ndns_19to64 <- readRDS(paste0(path_data,"/working/ndns_19to64_bloodanalytes_wh_f_a20u45_hb_np.RDS"))

colnames(nhanes_2013) <- c("caseid","age_in_months","gender","pregnancy_status",
                           "ethnicity","who_category","hb",
                           "median_nhanes","mean_nhanes","sd_nhanes")
nhanes_2013$age_in_years <- nhanes_2013$age_in_months/12
nhanes_2013$dataset <- "NHANES 2013-14"

colnames(ndns_y1to4) <- c("ethnicity","ethnicity_n","ethnicity_u","gender","age_in_years",
                          "hb","pregnancy_status","pregnancy_status_underage","pregnancy_status_now",
                          "iron_deficiency_drug","ferritin","ferritin_result",
                          "soluble_transferrin_receptor","soluble_transferrin_receptor_result")
ndns_y1to4$dataset <- "NDNS Y1to4"

colnames(ndns_19to64) <- c("caseid","gender","age_in_years","pregnancy_status","hb","ethnicity",
                           "plasma_iron","plasma_iron_binding_capacity","iron_saturation_perc",
                           "g_plasma_iron","g_plasma_iron_binding_capacity","g_iron_saturation_perc")
ndns_19to64$dataset <- "NDNS 19 to 64"

var_consolidated <- c("dataset","age_in_years","gender","hb","pregnancy_status")
consolidated <- do.call("rbind", list(nhanes_2013[,var_consolidated], 
                                      ndns_y1to4[,var_consolidated], 
                                      ndns_19to64[,var_consolidated]))

write.xlsx(as.data.frame(consolidated),
           file=paste0(path_data , "/working/evaluating_cutoffs_",Sys.Date(),".xlsx"),
           sheetName = "consolidated_wh",append=TRUE,row.names=FALSE)

write.xlsx(as.data.frame(ndns_19to64),
           file=paste0(path_data , "/working/evaluating_cutoffs_",Sys.Date(),".xlsx"),
           sheetName = "ndns_19to64_wh",append=TRUE,row.names=FALSE)

write.xlsx(as.data.frame(ndns_y1to4),
           file=paste0(path_data , "/working/evaluating_cutoffs_",Sys.Date(),".xlsx"),
           sheetName = "ndns_y1to4_wh",append=TRUE,row.names=FALSE)

write.xlsx(as.data.frame(nhanes_2013),
           file=paste0(path_data , "/working/evaluating_cutoffs_",Sys.Date(),".xlsx"),
           sheetName = "nhanes_2013_wh",append=TRUE,row.names=FALSE)

# 2. Pooled histogram -----------------------------------------------------

hist1 <- ggplot() + theme_bw()+ xlab("Haemoglobin") +
  ggtitle("Pooled distribution for non-pregnant women 15 to 49 (N=1106)")
hist1 <- hist1 + geom_histogram(data=consolidated,aes(hb)) 
hist1 + geom_vline(aes(xintercept= median(consolidated$hb)),col="blue") + 
  geom_vline(aes(xintercept = median(consolidated$hb)-1.65*sd(consolidated$hb)),col="red")

