###For Component Score Calculation & Vulnerability Plotting
rm(list=ls())

d <- read.csv(here("ref_code", "Aggregated_Spreadsheet_7.28 - Sheet1.csv"))

###The default is to create the vulnerability plot with everyone's tallies, if you prefer just use one person's
###tallies than use the next, commented line

##d=d[which(d$Scorer=="NB"),]

d<-d[c(2:5,7:10)]
long <- melt(setDT(d), id.vars = c("Species", "Sensitivity_or_Exposure", "Description", "Scorer"), variable.name = "bin")
long$weights<-NA
long$weights<-ifelse(long$bin=="bin1_low", 1, long$weights)
long$weights<-ifelse(long$bin=="bin2_moderate", 2, long$weights)           
long$weights<-ifelse(long$bin=="bin3_high", 3, long$weights)           
long$weights<-ifelse(long$bin=="bin4_veryhigh", 4, long$weights)   
names(long)[6]<-"Tally"
long[is.na(long)] <- 0
long<-long[rep(seq(nrow(long)), long$Tally),]
long<-as.data.frame(long)
long<-long[c(-6)]

Species_Means<- long %>% group_by(Species, Description, Sensitivity_or_Exposure) %>%
  summarise(mean = mean(weights),
            sd = sd(weights),
            n = n(), 
            se = sd / sqrt(n))

Species_Means$Component_Score<-NA

Unique<-as.data.frame(unique(Species_Means$Species))
names(Unique) <- c("Species")
List<-as.list(as.character(Unique$Species))
Vulnerability_Scores = data.frame(matrix(nrow = 0, ncol = 4)) 
names(Vulnerability_Scores)<-c("Species", "Sensitivity", "Exposure_Near", "Exposure_Far")

for (i in 1:length(List)){
  Species = List[i]
  Single_Species_Means<-Species_Means[which(Species_Means$Species==Species),]
  
  Single_Species_Means_S<-Single_Species_Means[which(Single_Species_Means$Sensitivity_or_Exposure=="sensitivity"),]
  Very_High_S<-as.numeric(nrow(Single_Species_Means_S[Single_Species_Means_S$mean>=3.5,]))
  High_S<-as.numeric(nrow(Single_Species_Means_S[Single_Species_Means_S$mean>=3,]))
  Moderate_S<-as.numeric(nrow(Single_Species_Means_S[Single_Species_Means_S$mean>=2.5,]))
  Component_Score_S<-NULL
  Component_Score_S<-ifelse(Very_High_S > 2, "Very_High", 
                            ifelse(High_S > 1, "High",
                                   ifelse(Moderate_S > 1, "Moderate", "Low")))
  
  
  Single_Species_Means_EN<-Single_Species_Means[which(Single_Species_Means$Sensitivity_or_Exposure=="exposure_Near"),]
  Very_High_EN<-as.numeric(nrow(Single_Species_Means_EN[Single_Species_Means_EN$mean>=3.5,]))
  High_EN<-as.numeric(nrow(Single_Species_Means_EN[Single_Species_Means_EN$mean>=3,]))
  Moderate_EN<-as.numeric(nrow(Single_Species_Means_EN[Single_Species_Means_EN$mean>=2.5,]))
  Component_Score_EN<-NULL
  Component_Score_EN<-ifelse(Very_High_EN > 2, "Very_High", 
                             ifelse(High_EN > 1, "High",
                                    ifelse(Moderate_EN > 1, "Moderate", "Low")))
  
  Single_Species_Means_EF<-Single_Species_Means[which(Single_Species_Means$Sensitivity_or_Exposure=="exposure_Far"),]
  Very_High_EF<-as.numeric(nrow(Single_Species_Means_EF[Single_Species_Means_EF$mean>=3.5,]))
  High_EF<-as.numeric(nrow(Single_Species_Means_EF[Single_Species_Means_EF$mean>=3,]))
  Moderate_EF<-as.numeric(nrow(Single_Species_Means_EF[Single_Species_Means_EF$mean>=2.5,]))
  Component_Score_EF<-NULL
  Component_Score_EF<-ifelse(Very_High_EF > 2, "Very_High", 
                             ifelse(High_EF > 1, "High",
                                    ifelse(Moderate_EF > 1, "Moderate", "Low")))
  Combined_Scores<-cbind(Species, Component_Score_S, Component_Score_EN, Component_Score_EF)
  Vulnerability_Scores<-rbind(Vulnerability_Scores, Combined_Scores)
}


Vulnerability_Scores$Component_Score_S<-as.character(Vulnerability_Scores$Component_Score_S)
Vulnerability_Scores$Component_Score_S<-factor(Vulnerability_Scores$Component_Score_S, 
                                               levels=c("Low", "Moderate", "High", "Very_High"), ordered=TRUE)

Vulnerability_Scores$Component_Score_EN<-as.character(Vulnerability_Scores$Component_Score_EN)
Vulnerability_Scores$Component_Score_EN<-factor(Vulnerability_Scores$Component_Score_EN, 
                                                levels=c("Low", "Moderate", "High", "Very_High"), ordered=TRUE)

Vulnerability_Scores$Component_Score_EF<-as.character(Vulnerability_Scores$Component_Score_EF)
Vulnerability_Scores$Component_Score_EF<-factor(Vulnerability_Scores$Component_Score_EF, 
                                                levels=c("Low", "Moderate", "High", "Very_High"), ordered=TRUE)

ggplot(Vulnerability_Scores, aes(x=Component_Score_EF, y=Component_Score_S, label=Species)) + 
  scale_x_discrete(drop=FALSE) + scale_y_discrete(drop=FALSE) +
  annotate('rect', xmin=3.5, xmax=4.5, ymin=2.5, ymax=4.5, fill='red') +
  annotate('rect', xmin=2.5, xmax=4.5, ymin=3.5, ymax=4.5, fill='red') +
  annotate('rect', xmin=0.5, xmax=1.5, ymin=0.5, ymax=3.5, fill='forestgreen') +
  annotate('rect', xmin=0.5, xmax=3.5, ymin=0.5, ymax=1.5, fill='forestgreen') +
  annotate('rect', xmin=0.5, xmax=1.5, ymin=3.5, ymax=4.5, fill='yellow') +
  annotate('rect', xmin=3.5, xmax=4.5, ymin=0.5, ymax=1.5, fill='yellow') +
  annotate('rect', xmin=1.5, xmax=2.5, ymin=1.5, ymax=3.5, fill='yellow') +
  annotate('rect', xmin=1.5, xmax=3.5, ymin=1.5, ymax=2.5, fill='yellow') +
  annotate('rect', xmin=2.5, xmax=3.5, ymin=2.5, ymax=3.5, fill='orange') +
  annotate('rect', xmin=1.5, xmax=2.5, ymin=3.5, ymax=4.5, fill='orange') +
  annotate('rect', xmin=3.5, xmax=4.5, ymin=1.5, ymax=2.5, fill='orange') +
  geom_vline(xintercept = 1:length(unique(Vulnerability_Scores$Component_Score_S))+0.5, lwd=1) + 
  geom_hline(yintercept = 1:length(unique(Vulnerability_Scores$Component_Score_S))+0.5, lwd=1) +
  geom_text_repel(size=3, segment.color=NA, max.overlaps=50) 
