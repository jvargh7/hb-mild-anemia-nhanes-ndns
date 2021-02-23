  #----------------------------------------------------------------------------
# Project             : Evaluating Hb cutoffs
# Objective           : Reading and cleaning NHANES and NDNS data
# Owner               : Jithin
# Created date        : 17/01/18
# Modified by         : 
# Last Modified Date  : 17/01/18
#
# Modules :
#     1. Reading and cleaning NHANES data
#     2. Reading and cleaning NDNS data
#----------------------------------------------------------------------------

library(extrafont)


# 1. Reading and cleaning NHANES data -------------------------------------
# path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"
path_data <- "C:/Cloud/OneDrive - Emory University/Papers/NHANES-NDNS/22_Evaluating Hb cutoffs/data"

for (year in seq(1999,2015,by=2)){
  df <- readRDS(paste0(path_data,"/working/nhanes_",year,"to",year+1,".RDS"))
  df$survey <- "nhanes"
  df$round <- paste0(year,"-",year+1)
  
  if(year==1999){
    nhanes = df
  }
  if(year>1999){
    nhanes = plyr::rbind.fill(nhanes,df)
  }
  rm(df)
}

nhanes <- nhanes[nhanes$health_status %in% c(1,2,3),]
nhanes %>% group_by(ethnicity) %>% summarise(mean(hb,na.rm=TRUE),sd(hb,na.rm=TRUE),n())



# 2. Reading and cleaning NDNS data ---------------------------------------

ndns_1 <- readRDS(paste0(path_data,"/working/ndns_19to64_2000-2001.RDS"))
ndns_2 <- readRDS(paste0(path_data,"/working/ndns_y1to4_2008-2012.RDS"))

ndns_1$survey <- "ndns"
ndns_2$survey <- "ndns"

ndns_1$round <- "2000-2001"
ndns_2$round <- "2008-2012"

ndns <- plyr::rbind.fill(ndns_1,ndns_2)
ndns$ethnicity <- with(ndns,ifelse(ethnicity %in% 
                                     c("chinese","south asian","other asian"),
                                       "asian",ethnicity))
ndns <- ndns[ndns$health_status %in% c(1,2,NA),]


# 3. Combined NDNS and NHANES ---------------------------------------------

consolidated <- plyr::rbind.fill(ndns,nhanes)
table(consolidated$ethnicity)/12412
table(consolidated$health_status)/12412
table(consolidated$survey)/12412

cutoff <- consolidated %>% group_by(ethnicity) %>% summarise(mean = mean(hb,na.rm=TRUE),
                                                   sd = sd(hb,na.rm=TRUE),
                                                   n= n())
cutoff$proposed <- with(cutoff,mean-1.65*sd)

# jpeg(filename="journal/ijmr/Figure2_revised_color.jpg",width=2000,height=2000)
dist1 <- ggplot() + theme_bw(base_size=16) +
  geom_density(data=consolidated[consolidated$ethnicity=="black",],aes(x=hb,fill="a"),alpha=0.7)+ 
  geom_density(data=consolidated[consolidated$ethnicity=="white",],aes(x=hb,fill="b"),alpha=0.7)+
  geom_density(data=consolidated[consolidated$ethnicity=="mexican and hispanic",],aes(x=hb,fill="c"),alpha=0.7)+ 
  geom_density(data=consolidated[consolidated$ethnicity=="asian",],aes(x=hb,fill="d"),alpha=0.8) +
  geom_vline(aes(xintercept = 12,color="cutoff"),linetype = 1,size=1.2) +
  scale_fill_manual(name = '', 
                                     values=c('a'='yellow','b'='red','c'='green','d'='blue'),
                                     labels = c('Black','White','Mexican and Hispanic','Asian')) +
  scale_color_manual(name="",values=c('cutoff'='black'),labels="WHO cut-off (12 g/dl)") +
  ylab("Proportion of Individuals") +
  xlab("Haemoglobin (g/dl)")  + 
  guides(color = guide_legend(order=2),
         fill = guide_legend(order=1)) +
  theme(axis.text = element_text(size=18,family="serif"),
        axis.title = element_text(size=18,family="serif"),
        # axis.ticks = element_(size=3),
        # axis.ticks.x.bottom = element_line(size=12),
        legend.text = element_text(size = 18,family="serif"),
        # legend.spacing.y = unit(5,"line"),
        # legend.box.margin = margin(b=10, 'pt'),
        legend.title = element_text(size = 18,family="serif"),
        legend.key = element_rect(size = 6, fill = "white", colour = NA),
        legend.key.size = unit(2,"line"),
        # legend.margin = margin(5,5,5,0),
        # legend.box = element_rect(color=NA,fill=NA),
        legend.position = c(0.2,0.7))
ggsave(dist1,filename="journal/ijmr/Figure2_revised_color.jpg",
       units="in",width=10,height=8)
# dev.off()


# dist1 <- dist1 + scale_fill_manual(name = 'Ethnicity', 
#                                      values=c('blue'='grey20','green'='grey40','red'='grey80','yellow'='grey90'),
#                                      labels = c('Black','White','Mexican and Hispanic','Asian')) 



d1 <- density(consolidated[consolidated$ethnicity=="black",]$hb)
d2 <- density(consolidated[consolidated$ethnicity=="white",]$hb)
d3 <- density(consolidated[consolidated$ethnicity=="mexican and hispanic",]$hb)
d4 <- density(consolidated[consolidated$ethnicity=="asian",]$hb)
# https://stackoverflow.com/questions/23913276/texture-in-barplot-for-7-bars-in-r
# http://r.789695.n4.nabble.com/plot-function-color-transparency-td4682424.html

jpeg(filename="journal/ijmr/Figure2_revised_bw.jpg",width=2000,height=2000)
par(mar=c(5, 7, 4, 4))
plot(d2,main="",xlab= "Haemoglobin (g/dl)", ylab= 'Proportion of Individuals',
     cex.lab=3, cex.axis=2, cex.main=4)

polygon(d2,col=adjustcolor("darkgrey")) #white

polygon(d3,col="white") #darkgrey transparent- mexican hispanic
polygon(d3,density=40,col="black",angle=150) #darkgrey transparent- mexican hispanic
polygon(d3,density=40,col="black",angle=30) #darkgrey transparent

polygon(d1,col=adjustcolor("lightgrey")) #stippled

polygon(d4,col=adjustcolor("grey",alpha=0.8),angle=45)  #light grey- Asian
polygon(d4,col=adjustcolor("black"),density=50,angle=45)  #light grey- Asian
abline(v = 12,lty=2,col=1)

legend("topleft",legend = c("White","Mexican and \nHispanic",
                            "Black","Asian"),
       fill=c("#A9A9A9FF","#FFFFFFFF","#D3D3D3FF","#BEBEBECC"),
       bty="n",
       ncol=2,cex=3,
       x.intersp = 0.1,y.intersp = 0.5)
par(bg="transparent")
legend("topleft",legend = c("White","Mexican and \nHispanic",
                            "Black","Asian"),
       fill=c("#A9A9A9FF","#000000","#D3D3D3FF","#000000FF"),
       density=c(0,40,0,50),
       angle=c(0,150,0,45),
       bty="n",
       ncol=2,cex=3,
       x.intersp = 0.1,y.intersp = 0.5)
par(bg="transparent")
legend("topleft",legend = c("White","Mexican and \nHispanic",
                            "Black","Asian"),
       fill=c("#A9A9A9FF","#000000","#D3D3D3FF","#000000FF"),
       density=c(0,40,0,50),
       angle=c(0,30,0,45),
       bty="n",
       ncol=2,cex=3,
       x.intersp = 0.1,y.intersp = 0.5)
par(bg="transparent")
legend(x=5.5,y=0.42,legend="WHO cut-off (12 g/dl)",lty=2,
       cex=3,
       bty="n",
       x.intersp = 0.1,y.intersp = 0.2)
dev.off()


# 4. Distribution of haemoglobin by round ----------------------------------

consolidated$survey_year <- with(consolidated,paste0(survey," ",round))

consolidated_race <- consolidated %>% 
  dplyr::mutate(survey_year = toupper(survey_year),
                ethnicity = toupper(ethnicity)) %>% 
  group_by(survey_year,ethnicity) %>% 
  summarise(n=n(),
            mean=mean(hb),
            lower = quantile(hb,probs=0.025),
            upper = quantile(hb,probs=0.975),
            sd = sd(hb),
            se = sd/sqrt(n))

consolidated_race$lower <- with(consolidated_race,mean-1.96*se)
consolidated_race$upper <- with(consolidated_race,mean+1.96*se)

library(forestplot)
forestplot(labeltext=consolidated_race$survey_year,
           mean=consolidated_race$mean,lower=consolidated_race$lower,consolidated_race$upper,
           col=fpColors(lines="darkblue",zero="red"))

consolidated_race$ethnicity <- as.factor(consolidated_race$ethnicity)
consolidated_race$survey_year <- toupper(consolidated_race$survey_year)
levels(consolidated_race$ethnicity) <- c("A. Asian","B. Black", "C. Mexican American","other","D. White")

jpeg(filename="journal/ijmr/Figure1_revised_orig.jpg",width=2000,height=1500)
dist2 <- ggplot(data=consolidated_race[consolidated_race$ethnicity!="other",]) + theme_bw()
dist2 <- dist2 + xlab("Survey rounds")  + ylab ("Haemoglobin (g/dl)")
dist2 <- dist2 + geom_point(aes(x=survey_year,y=mean,group=ethnicity),size=10) + facet_wrap(~ethnicity)
dist2 <- dist2 + geom_errorbar(aes(x=survey_year,ymin=lower,ymax=upper,group=ethnicity),size=1)
dist2 <- dist2 + theme(text=element_text(size=30), axis.text.x = element_text(angle = 90, hjust = 1))
dist2 <- dist2 + scale_y_continuous(limits=c(10,15))
print(dist2)
dev.off()


consolidated_race_jitter <- consolidated %>% 
  dplyr::mutate(survey_year = toupper(survey_year),
                ethnicity = toupper(ethnicity)) %>% 
  group_by(survey_year,ethnicity) %>% 
  summarise(n=n(),
            mean=mean(hb),
            # lower = quantile(hb,probs=0.025),
            # upper = quantile(hb,probs=0.975),
            sd = sd(hb),
            lower = mean - 1.96*sd,
            upper = mean + 1.96*sd) %>% 
  dplyr::filter(ethnicity!="OTHER")

# jpeg(filename="journal/ijmr/Figure1_revised_jitter.jpg",width=2000,height=1500)
ethnicity_names = c(ASIAN = "Asian",
             BLACK = "Black",
             'MEXICAN AND HISPANIC' = "Mexican and Hispanic",
             'WHITE' = "White")
fig1 <- consolidated %>% 
  dplyr::filter(ethnicity!="other") %>% 
  dplyr::mutate(survey_year = toupper(survey_year),
                ethnicity = toupper(ethnicity)) %>% 
ggplot(data=.,aes(x=survey_year,y=hb)) +
  theme_bw(base_size=16) +
  geom_jitter(aes(col="j"),height = 0,width=0.2,alpha=0.5) +
  facet_wrap(~ethnicity,labeller= labeller(ethnicity=ethnicity_names)) +
  geom_point(data=consolidated_race_jitter,
             aes(x=survey_year,y=mean,col="m"),size=3) +
  
  geom_errorbar(data=consolidated_race_jitter ,
             aes(x=survey_year,y=mean,ymin=lower,ymax=upper)) +
  theme(axis.title=element_text(size=18,family="serif"), 
        
        strip.text = element_text(size=18,family="serif"),
        axis.text.x = element_text(size=18,angle = 90, hjust = 1,family="serif"),
        axis.text.y = element_text(size=18,family="serif"),
        legend.text = element_text(size=18,family="serif"),
        legend.position="bottom") +
  xlab("Survey rounds")  + ylab ("Haemoglobin (g/dl)") +
  scale_color_manual("",
                     labels=c("j"="Individual","m"="Mean +/- 1.96 SD"),
                     values=c("j"="grey60","m"="black")) 
  
ggsave(fig1,filename="journal/ijmr/Figure1_revised_jitter.jpg",
       units="in",width=12,height=10)
# dev.off()

# 6. Bias in reporting health status? -------------------------------------

path_data <- "E:/OneDrive Jithin/OneDrive - St John's National Academy of Health Sciences/Tale of Two Cities/Papers/22_Evaluating Hb cutoffs/data"

for (year in seq(1999,2015,by=2)){
  df <- readRDS(paste0(path_data,"/working/nhanes_",year,"to",year+1,".RDS"))
  df$survey <- "nhanes"
  df$round <- paste0(year,"-",year+1)
  
  if(year==1999){
    nhanes = df
  }
  if(year>1999){
    nhanes = plyr::rbind.fill(nhanes,df)
  }
  rm(df)
}

nhanes$health_status_binary <- with(nhanes,ifelse(health_status %in% c(1,2,3),"Good","Poor"))


ndns_1 <- readRDS(paste0(path_data,"/working/ndns_19to64_2000-2001.RDS"))
ndns_2 <- readRDS(paste0(path_data,"/working/ndns_y1to4_2008-2012.RDS"))

ndns_1$survey <- "ndns"
ndns_2$survey <- "ndns"

ndns_1$round <- "2000-2001"
ndns_2$round <- "2008-2012"

ndns <- plyr::rbind.fill(ndns_1,ndns_2)
ndns$ethnicity <- with(ndns,ifelse(ethnicity %in% 
                                     c("chinese","south asian","other asian"),
                                   "asian",ethnicity))
ndns$health_status_binary <- with(ndns,ifelse(health_status %in% c(1,2,NA),"Good","Poor"))

chosen_vars <- c("survey","round","hb","health_status_binary","ethnicity")
consolidated_reported <- plyr::rbind.fill(nhanes[,chosen_vars],
                                    ndns[,chosen_vars])
racial_reporting <- consolidated_reported %>% group_by(ethnicity,health_status_binary) %>% tally() 
racial_reporting <- dcast(data=racial_reporting[racial_reporting$ethnicity!="other",],
                          ethnicity~health_status_binary)
racial_reporting$Poor/rowSums(racial_reporting[2:3])
chisq.test(as.matrix(racial_reporting[2:3]))
