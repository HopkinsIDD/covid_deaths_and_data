library(tidyverse)
library(ggplot2)
# install.packages("forecast")
# install.packages("xts")
#install.packages("npreg")
#install.packages("stats")
#install.packages('caret')
library(npreg)
#install.packages("ggpubr")
library(ggpubr)
library(forecast)
library(stats)
library(caret)

source("Deaths Visualization.R")

dat<-read_csv("data/CDC_CSSE_WeeklyDeaths.csv")
dat<-dat%>%
  filter(source=='csse')%>%
  select(incidD,state,date,USPS)

gold=read_csv(file="data/CSSE_WeeklyDeaths.csv")
gold<-gold%>% mutate(date=as.Date(date,format="%m/%d/%Y"),
                     Gold_Standard=factor(Gold_Standard))

gold_graphs<-FALSE #If we want to print graphs of gold standard outliers
if(gold_graphs){
  setwd("Graphs/Gold_Standard")
  
  abr="DC"
  pdf(paste("GOLD_STANDARD_",abr,".pdf",sep=""))
  gold%>%
    filter(USPS==abr)%>%
    ggplot()+geom_col(aes(x=date,y=incidD,fill=Gold_Standard))+
    labs(title=paste("GOLD_STANDARD_",abr))+
    coord_cartesian(ylim = c(0, max(gold %>% filter(USPS==abr) %>% select(incidD))))+
    theme(legend.position = "none")  
  
  dev.off()
  
}

dat<- left_join(dat,gold,by=c("USPS","date","state","incidD"))

spline_function<- function(date,incidD){
  mod.ss <- ss(date,incidD, nknots = 50)
  return(round(mod.ss$y))
}

moving_average<- function(data,w=3){
  mov_avg<-rep(0,length(data))
  for(i in ((w+1)/2) : (length(data) - (w-1)/2)){
    mov_avg[i]=(mean(data[(i-(w-1)/2):(i+(w-1)/2)]))
  }
  return(mov_avg)
}

# IQR OUT ----------------------------------------------------------------------
dat<-dat %>% 
  group_by(USPS)%>% 
  # mutate(iqr_out=factor(ifelse(incidD<quantile(incidD,0.25)-1.5*IQR(incidD) | 
  #                                incidD>quantile(incidD,0.75)+1.5*IQR(incidD),1,0)))
  mutate(iqr_out=factor(ifelse(incidD>quantile(incidD,0.75)+1.5*IQR(incidD),1,0)))

dat %>%
  ggplot()+geom_col(aes(x=date,y=incidD,fill=iqr_out))+
  coord_cartesian(ylim = c(0, 3000))+
  theme(legend.position = "none")+
  labs(title="Outlier Detection by Interquantile Range Method",x="Date",y="Deaths")+
  facet_wrap(~state)


# HAMPEL FILTER ----------------------------------------------------------------
dat<-dat %>% 
  group_by(USPS)%>% 
  # mutate(hampel_out=factor(ifelse(incidD<median(incidD)-3*mad(incidD) | 
  #                                   incidD>median(incidD)+3*mad(incidD),1,0)))
  mutate(hampel_out=factor(ifelse(incidD>median(incidD)+3*mad(incidD),1,0)))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=hampel_out))+
  coord_cartesian(ylim = c(0, 3000))+
  theme(legend.position = "none")+
  labs(title="Outlier Detection by Hampel Filter",x="Date",y="Deaths")+
  facet_wrap(~state)


# CARLINGA METHOD --------------------------------------------------------------
dat<-dat %>% 
  group_by(USPS)%>% 
  # mutate(carlinga_out=ifelse(incidD<median(incidD)-2.3*IQR(incidD) |
  #                              incidD>median(incidD)+2.3*IQR(incidD),1,0))
  mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(carlinga_out)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Carlinga Method",x="Date",y="Deaths")+
  facet_wrap(~state)

# SIMPLE PROPORTIONAL DIFFERENCE -----------------------------------------------
dat<-dat %>% 
  group_by(USPS) %>%
  mutate(mov_incidD=moving_average(incidD,3))

dat<-dat%>%
  mutate(incidD_movavg_simple=ifelse(incidD>2*mov_incidD,1,0))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(incidD_movavg_simple)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Proprtional Difference",x="Date",y="Deaths")+
  facet_wrap(~state)

# SIMPLE PROPORTIONAL DIFFERENCE + CARLINGA METHOD -----------------------------
dat<- dat %>%
  mutate(prop_diff_carlinga=incidD_movavg_simple*carlinga_out)

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(prop_diff_carlinga)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Proportional Difference and carlinga",x="Date",y="Deaths")+
  facet_wrap(~state)

# USING KERNEL FITTING AND SPLINE ESTIMATION -----------------------------------
# data=data.frame(incidD<-c(),state<-c(),date<-c(),
#                 USPS<-c(),spline_val<-c(),spline_err<-c(),kernel_val<-c())
# for (usps in unique(dat$USPS)){
#   n7<-dat%>%
#     filter(USPS==usps)
# 
#   # Kernel
#   kernel<-ksmooth(x=n7$date,y=n7$incidD,kernel="normal",bandwidth=20)
#   n7$kernel_val<-kernel$y
#   n7$kernel_err<-n7$incidD-kernel$y
# 
#   # Using Spline Fitting ---------------------------------------------------
#   mod.ss <- ss(n7$date, n7$incidD, nknots = 50)
#   n7$spline_val<-mod.ss$y
#   n7$spline_err<-(n7$incidD-mod.ss$y)
# 
#   data<-rbind(data,n7)
# 
# }

data<-dat %>%
  group_by(USPS)%>%
  mutate(spline_val=spline_function(date,incidD))

setwd("Graphs/Splines")

abr="PR"

pdf(paste("SPLINE_",abr,".pdf",sep=""))
data%>%
  #filter(USPS %in% c("FL","NY","MO","TN"))%>%
  filter(USPS %in% c(abr))%>%
  ggplot()+geom_col(aes(x=date,y=incidD))+
  geom_line(aes(x=date,y=spline_val,color="red"))+
  coord_cartesian(ylim = c(0, max(data %>% filter(USPS==abr) %>% pull(incidD))))+
  labs(title=paste("SPLINE_",abr,".pdf",sep=""),x="Date",y="Deaths")+
  theme(legend.position = "none")
  #facet_wrap(~state)

dev.off()

setwd("C:/Users/gupta/OneDrive/Documents/GitHub/covid_deaths_and_data")

data <-data%>%
  select(incidD,state,date,USPS,spline_err,kernel_err)

dat<-left_join(dat,data,by=c("incidD","state","date","USPS"))

dat<-dat %>% 
  #filter(USPS=="NY")%>%
  group_by(USPS)%>% 
  mutate(carlinga_out_spline_err=ifelse(spline_err<median(spline_err)-2.3*IQR(spline_err) |
                                                 spline_err>median(spline_err)+2.3*IQR(spline_err),1,0)) 



# POISSON ----------------------------------------------------------------------
# QPOIS ------------------------------------------------------------------------
p=0.99
dat<-dat %>%
  mutate(qpois_out = as.numeric(incidD > qpois(p,mov_incidD)))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(qpois_out)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier",x="Date",y="Deaths")+
  facet_wrap(~state)

# CONFIDENCE INTERVAL ----------------------------------------------------------
# STATOLOGY : https://www.statology.org/poisson-confidence-interval/------------
ci=99.9
p=1-(1-ci/100)/2

dat<-dat %>% 
  mutate(pois_out_statology = as.numeric(incidD > 0.5*qchisq(p= p, df=2*(mov_incidD +1))))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(pois_out_statology)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier",x="Date",y="Deaths")+
  facet_wrap(~state)

# POISSON 99.9% CI STATOLOGY + CARLINGA ------------------------------------------------------------
dat<- dat %>%
  mutate(pois_stat_99.9_carlinga=pois_out_statology*carlinga_out)

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Proportional Difference and carlinga",x="Date",y="Deaths")+
  facet_wrap(~state)

# POISSON 99.9% CI STATOLOGY + CARLINGA ------------------------------------------------------------
dat<- dat %>%
  mutate(pois_stat_99.9_carlinga_scaled=pois_out_statology*carlinga_out*incidD_movavg_simple)

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Proportional Difference and carlinga",x="Date",y="Deaths")+
  facet_wrap(~state)

# CONFUSION MATRIX -------------------------------------------------------------

cf_dat<-data.frame(USPS<-c(), Accuracy<-c(),Sensitivity<-c(),
                                    Specificity<-c(),Recall<-c())

for(abr in c(state.abb, "PR", "DC")){
temp<-dat%>%
  filter(USPS==abr)
  #filter(USPS=="AL")
d<-factor(temp$pois_out_statology)
r<-factor(temp$Gold_Standard)
levels(r)<-c(0,1)
levels(d)<-c(0,1)
cf<-confusionMatrix(data=d,reference=r)

Accuracy<-cf$overall[["Accuracy"]]
Sensitivity<-cf$byClass[["Sensitivity"]]
Specificity<-cf$byClass[["Specificity"]]
Precision<-cf$byClass[["Precision"]]
Recall<-cf$byClass[["Recall"]]

cf_dat<-rbind(cf_dat,cbind(abr,Accuracy,Sensitivity,Specificity,Recall))

}

print(paste("Maximum accuracy",max(cf_dat$Accuracy)))
print(paste("Minimum accuracy",min(cf_dat$Accuracy)))
print(paste("Mean accuracy",mean(as.numeric(cf_dat$Accuracy))))
print(paste("Median accuracy",median(as.numeric(cf_dat$Accuracy))))
print(nrow(cf_dat %>% filter(Accuracy==1)))


# DATA GAPS --------------------------------------------------------------------

info <- dat %>%
  select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled)

# temp<-info %>% 
#   filter(incidD>0)%>%
#   group_by(state)%>%
#   summarise(low=median(incidD)-2.3*IQR(incidD))

info<-info %>%
  group_by(USPS) %>%
  mutate(cum_incidD=cumsum(incidD))


info<-info %>%
  group_by(USPS)%>%
  mutate(cum_spline_val=spline_function(date,cum_incidD))

info%>%
  filter(USPS=="NY")%>%
  ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
  geom_line(aes(x=date,y=cum_spline_val),color="black")+
  theme(legend.position = "none")+
  #coord_cartesian(ylim = c(0, 3000))+
  facet_wrap(~USPS)

temp<- info%>% filter(USPS=="NY")
outlier_date<-temp$date[match(temp$pois_stat_99.9_carlinga_scaled,1,nomatch=0)==1]
outlier_date




