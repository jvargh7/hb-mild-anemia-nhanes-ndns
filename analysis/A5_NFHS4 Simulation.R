#----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NFHS-4 data
# Owner               : Jithin
# Created date        : 29/01/18
# Modified by         : 
# Last Modified Date  : 29/01/18
#
# Modules :
#     1. Reading NFHS-4 data
#     2. Cleaning and creating new variables
#----------------------------------------------------------------------------


# 1. Reading NFHS-4 data -------------------------------------

# path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"
path_data <- "C:/Cloud/OneDrive - Emory University/Papers/NHANES-NDNS/22_Evaluating Hb cutoffs/data"

for (i in 3:7){
  dt <- fread(paste0(path_nfhs4_ir_data,"/working/IAIR71FL_",i,".csv"))
  if(i==1){
    hb_nfhs4_ir <- copy(dt[,.(v002,v003,v005,sdistri, v012, v013, v024, v025, v042,v437,v438,v453,v454,v455,v456,v457,v458)])
  }
  if(i>1){
    dt_select <- copy(dt[,.(v002,v003,v005,sdistri, v012, v013, v024, v025, v042,v437,v438,v453,v454,v455,v456,v457,v458)])
    hb_nfhs4_ir <- rbind(hb_nfhs4_ir,dt_select)
  }
  rm(dt)
  
  print(paste0("Reading file","IAIR71FL_",i,".csv"))
  rm(dt_select)
}

saveRDS(hb_nfhs4_ir,file=paste0("data/working/hb_nfhs4_ir_v1.RDS"))


# 2. Cleaning and creating new variables ----------------------------------

hb_nfhs4_ir <- readRDS(file=paste0("data/working/hb_nfhs4_ir_v1.RDS"))
hb_nfhs4_ir <- hb_nfhs4_ir[,lapply(.SD,as.numeric)]

colnames(hb_nfhs4_ir) <- c("hhuid","resp_no","national_woman_weight","district","age","age_5yr",
                           "state","urban_rural","hh_selected_hb","weight_in_kg","height_in_cm",
                           "hb_in_g_dl","currently_pregnant","result_hb_measurement","hb_altitude_smoking_adj",
                           "anemia_level","anemia_agree_referral")


hb_nfhs4_ir <- hb_nfhs4_ir[,d_hb := ifelse(result_hb_measurement==0,hb_in_g_dl,NA)]
hb_nfhs4_ir <- hb_nfhs4_ir[,d_anemia_level := ifelse(anemia_level %in% c(1,2,3),1,0)]
hb_nfhs4_ir <- hb_nfhs4_ir[,d_anemia_who := ifelse(currently_pregnant==1,
                                                   ifelse(d_hb<110,1,0),
                                                   ifelse(d_hb<120,1,0))]

hb_nfhs4_ir <- hb_nfhs4_ir[,d_hb_adj := ifelse(result_hb_measurement==0,hb_altitude_smoking_adj,NA)]
hb_nfhs4_ir <- hb_nfhs4_ir[,d_anemia_adj_who := ifelse(currently_pregnant==1,
                                                   ifelse(d_hb_adj<110,1,0),
                                                   ifelse(d_hb_adj<120,1,0))]
hb_nfhs4_ir <- hb_nfhs4_ir[,d_cutoff_adj_who := ifelse(currently_pregnant==1,
                                                       ifelse(d_hb_adj<110,1,0),
                                                       ifelse(d_hb_adj<112.2,1,0))]



#Removing outliers
hb_nfhs4_ir <- hb_nfhs4_ir[between(d_hb,20,300,incbounds = TRUE) & 
                             between(d_hb_adj,20,300,incbounds = TRUE)]

hb_nfhs4_ir <- hb_nfhs4_ir[,d_mild_anemia_adj_who := ifelse(currently_pregnant==1,
                                                            ifelse(d_hb_adj %between% c(100,109),1,0),
                                                            ifelse(d_hb_adj %between% c(110,119),1,0))]

hb_nfhs4_ir <- hb_nfhs4_ir[,d_moderate_anemia_adj_who := ifelse(currently_pregnant==1,
                                                            ifelse(d_hb_adj %between% c(70,99),1,0),
                                                            ifelse(d_hb_adj %between% c(80,109),1,0))]

hb_nfhs4_ir <- hb_nfhs4_ir[,d_severe_anemia_adj_who := ifelse(currently_pregnant==1,
                                                                ifelse(d_hb_adj < 70,1,0),
                                                                ifelse(d_hb_adj < 80,1,0))]


hb_nfhs4_ir[,wtd.mean(d_anemia_level,w=national_woman_weight),by=currently_pregnant]
hb_nfhs4_ir[,wtd.mean(d_anemia_who,w=national_woman_weight),by=currently_pregnant]

hb_nfhs4_ir[,wtd.mean(d_anemia_adj_who,w=national_woman_weight),by=currently_pregnant]
hb_nfhs4_ir[,wtd.mean(d_mild_anemia_adj_who,w=national_woman_weight),by=currently_pregnant]
hb_nfhs4_ir[,wtd.mean(d_moderate_anemia_adj_who,w=national_woman_weight),by=currently_pregnant]
hb_nfhs4_ir[,wtd.mean(d_severe_anemia_adj_who,w=national_woman_weight),by=currently_pregnant]


hb_nfhs4_ir[,wtd.mean(d_cutoff_adj_who,w=national_woman_weight),by=currently_pregnant]

hb_nfhs4_ir[,wtd.mean(d_anemia_who,w=national_woman_weight),by=.(currently_pregnant,
                                                                     urban_rural)]
hb_nfhs4_ir[,wtd.mean(d_anemia_adj_who,w=national_woman_weight),by=.(currently_pregnant,
                                                                     urban_rural)]

#d_anemia_adj_who seems to be closer to India factsheet


# 3. State and district aggregates ----------------------------------------
state_district <- hb_nfhs4_ir[!duplicated(district),.SD,.SDcols= c("district","state")]
xlsx::write.xlsx(state_district,"data/working/nfhs4_layout.xlsx",append=TRUE,
                 sheetName = "state_district",row.names = FALSE)

df <- as.data.frame(hb_nfhs4_ir[,.(prevalence = wtd.mean(d_anemia_adj_who,w=national_woman_weight)*100,
                                   prevalence_proposed = wtd.mean(d_cutoff_adj_who,w=national_woman_weight)*100,
                                   mild = wtd.mean(d_mild_anemia_adj_who,w=national_woman_weight)*100,
                                   moderate = wtd.mean(d_moderate_anemia_adj_who,w=national_woman_weight)*100,
                                   severe = wtd.mean(d_severe_anemia_adj_who,w=national_woman_weight)*100),
                                by=.(state,district,currently_pregnant)])
df <- df[df$currently_pregnant==0,]

source("E:/tata-nin-data-repo/package/map_plot.R")

map_plot(df=df,
         value_var = "prevalence",
         id_var="district",
         district=TRUE,
         title="Anaemia Prevalence (%)",
         state=c("India"),
         legend="Percentage (%)",
         palette="Greys",
         direction=1)

map_plot(df=df,
         value_var = "prevalence_proposed",
         id_var="district",
         district=TRUE,
         title="Anaemia Prevalence (%)",
         state=c("India"),
         legend="Percentage (%)",
         palette="Greys",
         direction=1)
xlsx::write.xlsx(df,"data/working/nfhs4_layout.xlsx",append=TRUE,
                 sheetName = "district_prevalences",row.names = FALSE)


df_s <- as.data.frame(hb_nfhs4_ir[,.(prevalence = wtd.mean(d_anemia_adj_who,w=national_woman_weight)*100,
                                     prevalence_proposed = wtd.mean(d_cutoff_adj_who,w=national_woman_weight)*100,
                                     mild = wtd.mean(d_mild_anemia_adj_who,w=national_woman_weight)*100,
                                     moderate = wtd.mean(d_moderate_anemia_adj_who,w=national_woman_weight)*100,
                                     severe = wtd.mean(d_severe_anemia_adj_who,w=national_woman_weight)*100),
                                by=.(state,currently_pregnant)])
df_s <- df_s[df_s$currently_pregnant==0,]

state_name <- readxl::read_excel("E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data/working/nfhs4_layout.xlsx", 
                         sheet = "state_name")

df_s <- merge(df_s,state_name[,c("state_name","id","v024")],by.x="state",by.y="v024")
df_s$id <- as.character(df_s$id)
map_plot(df=df_s,
         value_var = "prevalence",
         id_var="id",
         district=FALSE,
         title="Anaemia Prevalence (%)",
         state=c("India"),
         legend="Percentage (%)",
         palette="Greys",
         direction=1)

map_plot(df=df_s,
         value_var = "prevalence_proposed",
         id_var="id",
         district=FALSE,
         title="Anaemia Prevalence (%)",
         state=c("India"),
         legend="Percentage (%)",
         palette="Greys",
         direction=1)


xlsx::write.xlsx(df_s,"data/working/nfhs4_layout.xlsx",append=TRUE,
                 sheetName = "state_prevalences",row.names = FALSE)

# 4. Histogram ------------------------------------------------------------

np_allindia <- as.data.frame(hb_nfhs4_ir[currently_pregnant==0,.SD,.SDcols=c("national_woman_weight",
                                                         "d_hb_adj")])

# jpeg(filename="journal/ijmr/Figure3.jpg",width=2000,height=2000)
# hist_np <- ggplot(np_allindia,aes(x=d_hb_adj/10,w=national_woman_weight)) + theme_bw()
# hist_np <- hist_np + geom_histogram(fill="lightgrey",col="black",bins=50) + xlab("Haemoglobin (in g/dl)") + ylab("Weighted frequency")
# 
# hist_np <- hist_np + geom_vline(xintercept = 12,aes(col="WHO cut-off"),col="black",
#                                 linetype="solid",show.legend =TRUE,size=2)
# hist_np <- hist_np + geom_vline(xintercept = 11.22,aes(col="Derived cut-off"),col="black",
#                                 linetype="dotted",show.legend =TRUE,size=2)
# hist_np <- hist_np + theme(text=element_text(size=30))
# hist_np
# dev.off()

# jpeg(filename="journal/ijmr/Figure3_revised.jpg",width=2000,height=2000)
fig3 <- np_allindia %>% 
ggplot(data = .,aes(x=d_hb_adj/10,w=national_woman_weight)) + theme_bw(base_size=16) +
  geom_histogram(fill="lightgrey",col="black",bins=50) + 
  xlab("Haemoglobin (in g/dl)") + ylab("Weighted frequency") +
  geom_vline(aes(xintercept = 12,linetype="s"),size=2) + 
  geom_vline(aes(xintercept = 11.22,linetype="d"),size=2) +
  theme(axis.text=element_text(size=18,family="serif"),
        axis.title=element_text(size=18,family="serif"),
        legend.text = element_text(size=18,family="serif",margin = margin(b=50,unit="pt")),
        legend.key.size = unit(1,"line"),
        legend.position = c(0.2,0.7)) +
  guides(linetype = guide_legend(label.vjust = -10,default.unit = "pt")) + 
  scale_linetype_manual("",
                        values=c("s"=1,"d"=3),
                        labels=c("Derived cut-off (11.22 g/dl)","WHO cut-off (12 g/dl)"))
ggsave(fig3,filename="journal/ijmr/Figure3_revised.jpg",
       units="in",width=12,height=10)
# dev.off()