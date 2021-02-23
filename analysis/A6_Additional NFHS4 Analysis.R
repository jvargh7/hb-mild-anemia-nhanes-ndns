#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NFHS-4 data
# Owner               : Jithin
# Created date        : 14/02/18
# Modified by         : 
# Last Modified Date  : 14/02/18
#
# Modules :
#     1. Reading NFHS-4 data
#     2. Cleaning and creating new variables
#----------------------------------------------------------------------------


# 1. Reading NFHS-4 data --------------------------------------------------

path_hb_distributions_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/publications-support/hb_distributions/data"
hb_nfhs4_ir <- readRDS(paste0(path_hb_distributions_data,"/working/hb_distribution_nfhs4_ir_v2.RDS"))

xtabs(~hb_nfhs4_ir$wealth_index)

wealth_ventile <- hb_nfhs4_ir  %>% do(data.frame(t(quantile(.$wealth_index_factor_score,
                                                            w=.$national_woman_weight,
                                                            probs =seq(0,1,0.05)))))

colnames(wealth_ventile) <- str_replace_all(colnames(wealth_ventile),"X","w_")
colnames(wealth_ventile) <- str_replace_all(colnames(wealth_ventile),"\\.","")

wealth_ventile_vec <- unlist(wealth_ventile)

wtd_var <- function(value,weights,na.rm=TRUE){
  mean_value = wtd.mean(value,w=weights,na.rm=TRUE)
  sum_weights = sum(weights,na.rm = TRUE)
  numerator = sum(weights*(value-mean_value)^2,na.rm = TRUE)*length(weights)
  denominator = sum_weights*(length(weights)-1)
  var_value = numerator/denominator
  return(var_value)
}


hb_nfhs4_ir$wealth_ventile <- cut(hb_nfhs4_ir$wealth_index_factor_score,breaks=wealth_ventile_vec,
                                  include.lowest = TRUE,labels=FALSE)
hb_nfhs4_ir[wealth_ventile %in% c(19,20),
            .(mean=wtd.mean(d_hb_adj,w=national_woman_weight),
              sd=sqrt(wtd_var(d_hb_adj,w=national_woman_weight))),
            by=currently_pregnant==1]

ventile_table <- hb_nfhs4_ir[,
                             .(mean=wtd.mean(d_hb_adj,w=national_woman_weight),
                               sd=sqrt(wtd_var(d_hb_adj,w=national_woman_weight))),
                             by=.(currently_pregnant==1,wealth_ventile)]

11.88 - 1.65*1.52
write.csv(ventile_table,"writing/ventile_table.csv")


# 2. Deciles --------------------------------------------------------------



wealth_decile <- hb_nfhs4_ir  %>% do(data.frame(t(quantile(.$wealth_index_factor_score,
                                                            w=.$national_woman_weight,
                                                            probs =seq(0,1,0.1)))))

colnames(wealth_decile) <- str_replace_all(colnames(wealth_decile),"X","w_")
colnames(wealth_decile) <- str_replace_all(colnames(wealth_decile),"\\.","")

wealth_decile_vec <- unlist(wealth_decile)

wtd_var <- function(value,weights,na.rm=TRUE){
  mean_value = wtd.mean(value,w=weights,na.rm=TRUE)
  sum_weights = sum(weights,na.rm = TRUE)
  numerator = sum(weights*(value-mean_value)^2,na.rm = TRUE)*length(weights)
  denominator = sum_weights*(length(weights)-1)
  var_value = numerator/denominator
  return(var_value)
}


hb_nfhs4_ir$wealth_decile <- cut(hb_nfhs4_ir$wealth_index_factor_score,breaks=wealth_decile_vec,
                                  include.lowest = TRUE,labels=FALSE)
hb_nfhs4_ir[wealth_decile %in% c(10),
            .(mean=wtd.mean(d_hb_adj,w=national_woman_weight),
              sd=sqrt(wtd_var(d_hb_adj,w=national_woman_weight))),
            by=currently_pregnant==1]

decile_table <- hb_nfhs4_ir[,
                             .(mean=wtd.mean(d_hb_adj,w=national_woman_weight),
                               sd=sqrt(wtd_var(d_hb_adj,w=national_woman_weight))),
                             by=.(currently_pregnant==1,wealth_decile)]

11.88 - 1.65*1.52
write.csv(decile_table,"writing/decile_table.csv")
