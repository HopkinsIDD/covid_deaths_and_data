################################################################################
### ------------ This consists of all outlier detection methods ------------ ###
################################################################################

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

#FUNCTIONS  FOR OUTLIER DETECTION WHICH WILL BE NEEDED LATER
spline_function<- function(date,incidD,weights=rep(1,length(incidD))){
  mod.ss <- ss(date,incidD, w= weights)
  return(round(mod.ss$y))
}

moving_average<- function(data,w=3){
  mov_avg<-rep(0,length(data))
  for(i in ((w+1)/2) : (length(data) - (w-1)/2)){
    mov_avg[i]=(mean(data[(i-(w-1)/2):(i+(w-1)/2)]))
  }
  return(mov_avg)
}

cum_diff<-function(vec){
  vec_diff<-c()
  for(i in 1:length(vec)-1){
    vec_diff<-c(vec_diff,vec[i+1]-vec[i])
    
  }
  vec_diff<-c(vec[1],vec_diff)
  return(vec_diff)
}

wave_number<-function(date,incidD){
  temp<- data.frame(date,incidD) %>%
    mutate(spline_spar=round((ss(date,incidD,spar =0))$y),
           slp=c(0,diff(spline_spar,lag=2),0),
           wave_number=rep(1,length(incidD)))
  wave=1
  for (i in 1:(nrow(temp)-1)){
    temp$wave_number[i]<-wave
    if(temp$slp[i]<0 & temp$slp[i+1]>=0){
      wave=wave+1
    }
  }
  temp$wave_number[i+1]<-max(temp$wave_number)
  return(temp$wave_number)
}

# IMPORTING DEATH DATA ---------------------------------------------------------
dat<- read.csv("final/data/CSSE_WeeklyDeaths_DIRECT_DOWNLOAD.csv")
dat<- dat %>%
  select(-c(datew)) %>%
  mutate(date=lubridate::as_date(as.Date(`date`, format="%m/%d/%Y")))

graphs= FALSE #Variable which represents if we want to print graphs

ggarrange(dat %>%
  filter(USPS=="FL",date>="2021-01-01" & date<="2021-12-31") %>%
  mutate(spline_spar=round((ss(date,incidD,lambda =0))$y)) %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,color="red",fill="red"))+
  geom_line(aes(x=date,y=spline_spar)) +
  theme(legend.position = "none")+
  labs(x="Date",y="Deaths",title="Lambda=0"),
  dat %>%
    filter(USPS=="FL",date>="2021-01-01" & date<="2021-12-31") %>%
    mutate(spline_spar=round((ss(date,incidD,lambda =0.000001))$y)) %>%
    ggplot()+
    geom_col(aes(x=date,y=incidD,color="red",fill="red"))+
    geom_line(aes(x=date,y=spline_spar)) +
    theme(legend.position = "none")+
    labs(x="Date",y="Deaths",title="Lambda=0.000001"),
  dat %>%
    filter(USPS=="FL",date>="2021-01-01" & date<="2021-12-31") %>%
    mutate(spline_spar=round((ss(date,incidD,lambda =0.0001))$y)) %>%
    ggplot()+
    geom_col(aes(x=date,y=incidD,color="red",fill="red"))+
    geom_line(aes(x=date,y=spline_spar)) +
    theme(legend.position = "none")+
    labs(x="Date",y="Deaths",title="Lambda=0.0001"),
  dat %>%
    filter(USPS=="FL",date>="2021-01-01" & date<="2021-12-31") %>%
    mutate(spline_spar=round((ss(date,incidD,lambda =0.01))$y)) %>%
    ggplot()+
    geom_col(aes(x=date,y=incidD,color="red",fill="red"))+
    geom_line(aes(x=date,y=spline_spar)) +
    theme(legend.position = "none")+
    labs(x="Date",y="Deaths",title="Lambda=0.01"),
  nrow = 2,ncol = 2)


#Performing wave identification
dat<-dat %>%
  group_by(USPS) %>%
  mutate(wave=factor(wave_number(date,incidD)))
if(graphs){
pdf(file= "final/FINAL_Deaths_waves.pdf" )
for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(dat %>%
          filter(USPS==abr) %>%
          ggplot()+
          geom_col(aes(x=date,y=incidD,fill=wave)) +
          geom_line(aes(x=date,y=spline_spar)))
}
dev.off()

}





# OUTLIER DETECTION METHODS BELOW ----------------------------------------------

# METHOD 1: IQR OUT ------------------------------------------------------------
dat<-dat %>% 
  group_by(USPS,wave)%>% 
  mutate(iqr_out=ifelse(incidD>quantile(incidD,0.75)+1.5*IQR(incidD),1,0))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Interquantile Range Method.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(dat %>%
    filter(USPS==abr) %>% 
    ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(iqr_out)))+
    #coord_cartesian(ylim = c(0, 3000))+
    theme(legend.position = "none")+
    labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

# METHOD 2: HAMPEL FILTER -----------------------------------------------------
dat<-dat %>% 
  group_by(USPS,wave)%>% 
  mutate(hampel_out=ifelse(incidD>median(incidD)+3*mad(incidD),1,0))


if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Hampel Filter.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(hampel_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

# METHOD 3:CARLINGA METHOD -----------------------------------------------------
dat<-dat %>% 
  group_by(USPS,wave)%>% 
  mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Carlinga Method.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(carlinga_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}


# METHOD 4: SCALED PROPORTIONAL DIFFERENCE -------------------------------------
dat<-dat %>% 
  group_by(USPS,wave) %>%
  mutate(mov_incidD=moving_average(incidD,3))

dat<-dat%>%
  mutate(scaled_prop_diff_out=ifelse(incidD>2*mov_incidD,1,0))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Scaled Proprtional Difference.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(scaled_prop_diff_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}


# # METHOD 5: SPLINE FITTING -----------------------------------------------------
# dat<-dat %>%
#   group_by(USPS,wave)%>%
#   mutate(spline_val=spline_function(date,incidD),
#          spline_val=ifelse(is.nan(spline_val),0,spline_val),
#          spline_diff=spline_val-incidD,
#          spline_carlinga_out=ifelse(spline_diff>median(spline_diff)+2.3*IQR(spline_diff),1,0))
# 
# if(graphs){
#   pdf(file= "final/Outlier Detection by Carlinga Method on Spline Difference.pdf" )
#   for (abr in  c(state.abb, "PR", "DC","AL_H")){
#     print(dat %>%
#             filter(USPS==abr) %>%
#             ggplot()+geom_col(aes(x=date,y=incidD,fill=spline_carlinga_out))+
#             #coord_cartesian(ylim = c(0, 3000))+
#             theme(legend.position = "none")+
#             labs(x="Date",y="Deaths"))#+
#     #facet_wrap(~state))
#   }
#   dev.off()
# }


# METHOD 6: POISSON DISTRIBUTION MAPPING ---------------------------------------

# QPOIS PROP = 0.95 ------------------------------------------------------------
prop=0.95
dat<-dat %>%
  group_by(USPS,wave)%>%
  mutate(qpois_0.95_out = as.numeric(incidD > qpois(prop,mov_incidD)))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Poisson Distribution Mapping QPOIS 0.95.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(qpois_0.95_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

# QPOIS PROP = 0.99 ------------------------------------------------------------
prop=0.99
dat<-dat %>%
  group_by(USPS,wave)%>%
  mutate(qpois_0.99_out = as.numeric(incidD > qpois(prop,mov_incidD)))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Poisson Distribution Mapping QPOIS 0.99.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(qpois_0.99_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}


# CONFIDENCE INTERVAL  95.0 ----------------------------------------------------
# STATOLOGY : https://www.statology.org/poisson-confidence-interval/------------
ci=95.0
prop=1-(1-ci/100)/2

dat<-dat %>% 
  group_by(USPS,wave)%>%
  mutate(pois_ci_95_out= as.numeric(incidD > 0.5*qchisq(p= prop, df=2*(mov_incidD +1))))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Poisson Distribution Mapping CI 95.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_ci_95_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

# CONFIDENCE INTERVAL  99.9 ----------------------------------------------------
# STATOLOGY : https://www.statology.org/poisson-confidence-interval/------------
ci=99.9
prop=1-(1-ci/100)/2

dat<-dat %>% 
  group_by(USPS,wave)%>%
  mutate(pois_ci_99.9_out= as.numeric(incidD > 0.5*qchisq(p= prop, df=2*(mov_incidD +1))))

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Poisson Distribution Mapping CI 99.9.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_ci_99.9_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

# METHOD 7: COMBINATION METHODS ------------------------------------------------

# (i) SCALED PROPORTIONAL DIFFERENCE + CARLINGA METHOD -------------------------

dat<- dat %>%
  group_by(USPS,wave)%>%
  mutate(scaled_prop_diff_carlinga_out=scaled_prop_diff_out*carlinga_out)

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Combination Method Scaled Proportional Difference and Carlinga.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(scaled_prop_diff_carlinga_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}


# (ii) POISSON CONFIDENCE INTERVAL  99.9 + CARLINGA METHOD ---------------------
dat<- dat %>%
  group_by(USPS,wave)%>%
  mutate(pois_ci_99.9_carlinga_out=pois_ci_99.9_out*carlinga_out)

if(graphs){
  pdf(file= "final/outlier/Outlier Detection by Combination Method Poisson 99.9 & CI and Carling.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_ci_99.9_carlinga_out)))+
            #coord_cartesian(ylim = c(0, 3000))+
            theme(legend.position = "none")+
            labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

# (iii) SCALED PROP DIFF + POISSON CONFIDENCE INTERVAL 99.9 + CARLINGA METHOD --
dat<- dat %>%
  group_by(USPS,wave)%>%
  mutate(scaled_pois_ci_99.9_carlinga_out=scaled_prop_diff_out*pois_ci_99.9_out*carlinga_out)

if(graphs){
  # pdf(file= "final/outlier/Outlier Detection by Combination Method Scaled Prop Diff and Poisson 99.9 & CI and Carlinga.pdf" )
  pdf(file= "final/Model Break Point Dates.pdf" )
  for (abr in  c(state.abb, "PR", "DC","AL_H")){
    print(dat %>%
            filter(USPS==abr) %>% 
            ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(scaled_pois_ci_99.9_carlinga_out)))+
            geom_col(aes(x = date, y = incidD, fill = factor(scaled_pois_ci_99.9_carlinga_out))) + 
            geom_vline(aes(xintercept = as.Date("2021-5-15", format = "%Y-%m-%d")), color = "dark green", size = 1) +
            geom_vline(aes(xintercept = as.Date("2021-9-25", format = "%Y-%m-%d")), color = "brown", size = 1) +
            geom_vline(aes(xintercept = as.Date("2021-12-11", format = "%Y-%m-%d")), color = "black", size = 1) +
            # scale_color_manual(values = c("dark green", "brown", "black"), 
            #                    name = "End of Time Frames",
            #                    labels = c("May 15, 2021", "September 9, 2021", "December 11, 2021")) +
            # labs(x = "Date", y = "Reported Deaths") +
            # theme(legend.position = "bottom")
            guides(fill = "none", color = guide_legend(title = "Lockdown dates")) +
            labs(x = "Date", y = "Reported Deaths",color = "Lockdown dates") +
            theme(legend.position = "bottom"))
            #coord_cartesian(ylim = c(0, 3000))+
            #theme(legend.position = "none")+
            # labs(title=abr,x="Date",y="Deaths"))#+
    #facet_wrap(~state))
  }
  dev.off()
}

dat<- dat %>%
  ungroup() %>%
  select(c(date,USPS,incidD,state,contains("out")))

dat_t<-dat
dat_t$state<-factor(dat$state,levels = unique(dat$state),ordered=TRUE)
dat_t %>%
  filter(USPS %in% c("DE","AR","MD","MO","NE","OK")) %>%
  #filter(USPS %in% c("MD")) %>%
  ggplot() + 
  geom_col(aes(x = date, y = incidD, fill = factor(scaled_pois_ci_99.9_carlinga_out))) + 
  geom_vline(aes(xintercept = as.Date("2021-5-15", format = "%Y-%m-%d"), color = "May 15, 2021"), size = 1.2,lty=5) +
  geom_vline(aes(xintercept = as.Date("2021-6-19", format = "%Y-%m-%d"), color = "June 19, 2021"), size = 1.2,lty=5) +
  #geom_vline(aes(xintercept = as.Date("2021-12-11", format = "%Y-%m-%d"), color = "December 11, 2021"), size = 1) +
  scale_color_manual(values = c("dark green", "black"), 
                     name = "End of time frames") +
  guides(fill = "none") +
  labs(x = "Date", y = "Reported Deaths") +
  theme(legend.title=element_blank(),
        legend.position=c(0.95, 1),
        legend.direction="vertical",
        legend.box="vertical",
        legend.key.size=unit(0.6, "cm"),
        legend.text=element_text(size=12))+
  facet_wrap(~state,scales = "free_y",nrow = 3, ncol = 2)



dat_t %>% 
  filter(USPS %in% c("DE","AR","MD","MO","NE","OK")) %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(wave)))+
  theme(legend.position = c(0.99,0.9))+
  scale_fill_discrete(name="Wave")+ 
  labs(x="Date",y="Reported Deaths")+
  facet_wrap(~state,scales = "free_y",nrow = 3, ncol = 2)


################################################################################

gold<- read.csv("final/data/CSSE_WeeklyDeaths_Gold_Outliers.csv")

gold<- gold %>%
  mutate(date=lubridate::as_date(as.Date(`date`, format="%Y-%m-%d")))

dat<-dat %>%
  left_join(gold,by=c("date","USPS","incidD"))

# CONFUSION MATRIX -------------------------------------------------------------
gold_test=TRUE #INDICATING If accuracy has to be checked for different methods
if(gold_test){
for(column_name in colnames(dat %>% select(contains("out")))){
cf_dat<-data.frame(USPS<-c(), Accuracy<-c(),Sensitivity<-c(),
                                    Specificity<-c(),Recall<-c())

for(abr in c(state.abb, "PR", "DC","AL_H")){
temp<-dat%>%
  filter(USPS==abr)
  #filter(USPS=="AL")
d<-factor(temp[[column_name]])
r<-factor(temp$gold_out)
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

print("++++++++++++++++++++++++++++++++")
print(column_name)
print(paste("Maximum accuracy",max(cf_dat$Accuracy)))
print(paste("Minimum accuracy",min(cf_dat$Accuracy)))
print(paste("Mean accuracy",mean(as.numeric(cf_dat$Accuracy))))
print(paste("Median accuracy",median(as.numeric(cf_dat$Accuracy))))
print(nrow(cf_dat %>% filter(Accuracy==1)))
print("++++++++++++++++++++++++++++++++")
}
}





# # DATA GAPS --------------------------------------------------------------------
# 
# info <- dat %>%
#   select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled)
# 
# # temp<-info %>% 
# #   filter(incidD>0)%>%
# #   group_by(state)%>%
# #   summarise(low=median(incidD)-2.3*IQR(incidD))
# 
# info<-info %>%
#   group_by(USPS) %>%
#   mutate(cum_incidD=cumsum(incidD))
# 
# # SPLINE RATIO METHOD ----------------------------------------------------------
# 
# info<-info %>%
#   group_by(USPS)%>%
#   mutate(cum_spline_val=spline_function(date,cum_incidD))
# 
# info<- info %>% filter(pois_stat_99.9_carlinga_scaled %in% c(1,0)) 
# 
# 
# final<- info %>% filter(USPS=="FLAG")
# 
# for (abr in  c(state.abb, "PR", "DC")){
# temp<- info%>% filter(USPS==abr) %>% mutate(diff=cum_incidD-cum_spline_val)
# 
# for (h in nrow(temp):1){
#   if(temp$pois_stat_99.9_carlinga_scaled[h]==1){
#     p=h
#     extra=temp$diff[p]
#     arr=c()
#     for(i in (p-1):1){
#       if(temp$diff[i]>0){
#         break
#       }
#       arr=c(arr,temp$diff[i])
#     }
#     arr=abs(ceiling(abs(arr)/sum(arr)*extra))
#     temp$incidD[p]=temp$incidD[p]-sum(arr)
#     k=1
#     for (i in (p-1):1){
#       if(temp$diff[i]>0){
#         break
#       }
#       temp$incidD[i]=temp$incidD[i]+arr[k]
# 
#       k=k+1
#     }
#   }
# }
# 
# #temp <- temp %>% select(-diff)
# 
# final<-rbind(final,temp)
# 
# }
# 
# final<-final %>% rename(incidD_new=incidD)
# final<- left_join(final,info,
#                   by=c("state","USPS","date","pois_stat_99.9_carlinga_scaled","cum_incidD","cum_spline_val"))
# 
# final<- final %>%
#   group_by(USPS) %>%
#   mutate(cum_incidD_new=cumsum(incidD_new))
# 
# 
# abr="AR"
# temp<- final %>% filter(USPS==abr)
# p1<-final%>%
#   filter(USPS==abr)%>%
#   ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   theme(legend.position = "none")+
#   #coord_cartesian(ylim = c(0, 3000))+
#   facet_wrap(~USPS)
# 
# p2<-final%>%
#   filter(USPS==abr)%>%
#   ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   geom_line(aes(x=date,y=cum_spline_val),color="black")+
#   theme(legend.position = "none")+
#   #coord_cartesian(ylim = c(0, 3000))+
#   facet_wrap(~USPS)
# 
# p3<-final%>%
#   filter(USPS==abr)%>%
#   ggplot()+geom_col(aes(x=date,y=cum_incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   geom_line(aes(x=date,y=cum_spline_val),color="black")+
#   labs(title="Corrected ")+
#   theme(legend.position = "none")+
#   #coord_cartesian(ylim = c(0, 3000))+
#   facet_wrap(~USPS)
# 
# 
# ggarrange(p1,p2,p3, ncol = 1, nrow = 3)
# 
# 
# 
# pdf(file= "SPLINE_RATIO_METHOD_UP_NEW.pdf" )
# for (abr in  c(state.abb, "PR", "DC")){
# print(ggarrange(final%>%
#             filter(USPS==abr)%>%
#             ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             theme(legend.position = "none")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           final%>%
#             filter(USPS==abr)%>%
#             ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             geom_line(aes(x=date,y=cum_spline_val),color="black")+
#             theme(legend.position = "none")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           final%>%
#             filter(USPS==abr)%>%
#             ggplot()+geom_col(aes(x=date,y=cum_incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             geom_line(aes(x=date,y=cum_spline_val),color="black")+
#             labs(title="Corrected ")+
#             theme(legend.position = "none")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS)
#           ,
#           final %>%
#             filter(USPS==abr)%>%
#             ggplot()+geom_col(aes(x=date,y=incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             theme(legend.position = "none")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           ncol = 1, nrow = 4))
# 
# }
# 
# 
# 
# dev.off()
# 
# # LOOPING PROCESS --------------------------------------------------------------
# 
# #Checking Graphs
# ci=99.9
# p=1-(1-ci/100)/2
# 
# 
# test<-final %>% 
#   select(incidD_new,USPS,date) %>%
#   rename(incidD=incidD_new) %>%
#   group_by(USPS) %>%
#   mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0),
#          mov_incidD=moving_average(incidD,3),
#          pois_out_statology = as.numeric(incidD > 0.5*qchisq(p= p, df=2*(mov_incidD +1))),
#          incidD_movavg_simple=ifelse(incidD>2*mov_incidD,1,0),
#          pois_stat_99.9_carlinga_scaled=pois_out_statology*carlinga_out*incidD_movavg_simple,
#          cum_incidD=cumsum(incidD),
#          cum_spline_val=spline_function(date,cum_incidD))%>%
#   select(incidD,date,USPS,pois_stat_99.9_carlinga_scaled,cum_incidD,cum_spline_val)
# 
# abr="FL"
# test%>%
#   filter(USPS==abr) %>%
#   ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   geom_line(aes(x=date,y=cum_spline_val),color="black")+
#   labs(title="Corrected ")+
#   theme(legend.position = "none")+
#   #coord_cartesian(ylim = c(0, 3000))+
#   facet_wrap(~USPS)
# 
# 
# 
# # START OF LOOPING -------------------------------------------------------------
# info<-dat %>%
#   select(incidD,USPS,date)
# 
# iteration=0
# while(TRUE){
#   iteration=iteration+1
#   ci=99.9
#   p=1-(1-ci/100)/2
#   print(iteration)
#   info<- info %>%
#     group_by(USPS) %>%
#     mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0),
#            mov_incidD=moving_average(incidD,3),
#            pois_out_statology = as.numeric(incidD > 0.5*qchisq(p= p, df=2*(mov_incidD +1))),
#            incidD_movavg_simple=ifelse(incidD>2*mov_incidD,1,0),
#            pois_stat_99.9_carlinga_scaled=pois_out_statology*carlinga_out*incidD_movavg_simple,
#            cum_incidD=cumsum(incidD),
#            cum_spline_val=spline_function(date,cum_incidD))%>%
#     select(incidD,date,USPS,pois_stat_99.9_carlinga_scaled,cum_incidD,cum_spline_val) %>%
#     filter(pois_stat_99.9_carlinga_scaled %in% c(1,0)) 
#   
#   final<- info %>% filter(USPS=="FLAG")
#   
#   for (abr in  c(state.abb, "PR", "DC")){
#     temp<- info%>% filter(USPS==abr) %>% mutate(diff=cum_incidD-cum_spline_val)
#     
#     for (h in nrow(temp):1){
#       if(temp$pois_stat_99.9_carlinga_scaled[h]==1){
#         p=h
#         extra=temp$diff[p]
#         arr=c()
#         for(i in (p-1):1){
#           if(temp$diff[i]>0){
#             break
#           }
#           arr=c(arr,temp$diff[i])
#         }
#         arr=abs(ceiling(abs(arr)/sum(arr)*extra))
#         temp$incidD[p]=temp$incidD[p]-sum(arr)
#         k=1
#         for (i in (p-1):1){
#           if(temp$diff[i]>0){
#             break
#           }
#           temp$incidD[i]=temp$incidD[i]+arr[k]
#           
#           k=k+1
#         }
#       }
#     }
#     
#     #temp <- temp %>% select(-diff)
#     
#     final<-rbind(final,temp)
#     
#   }
#   
#   final<-final %>% rename(incidD_new=incidD)
#   final<- left_join(final,info,
#                     by=c("USPS","date","pois_stat_99.9_carlinga_scaled","cum_incidD","cum_spline_val"))
#   
#   final<- final %>%
#     group_by(USPS) %>%
#     mutate(cum_incidD_new=cumsum(incidD_new))
#   
#   if(any(final$pois_stat_99.9_carlinga_scaled==1)==FALSE){
#     break
#   }
#   
#   info<-final %>% 
#     select(incidD_new,USPS,date) %>%
#     rename(incidD=incidD_new)
# }
# 
# dat<- dat %>%
#   group_by(USPS) %>%
#   mutate(cum_incidD=cumsum(incidD))
# 
# 
# pdf(file= "SPLINE_RATIO_METHOD_LOOP.pdf" )
# for (abr in  c(state.abb, "PR", "DC")){
#   print(ggarrange(dat%>%
#                     filter(USPS==abr)%>%
#                     ggplot()+geom_col(aes(x=date,y=incidD))+
#                     theme(legend.position = "none")+
#                     #coord_cartesian(ylim = c(0, 3000))+
#                     labs(title="ORIGINAL",x="Date",y="Number of Deaths")+
#                     facet_wrap(~USPS),
#                   dat%>%
#                     filter(USPS==abr)%>%
#                     ggplot()+geom_col(aes(x=date,y=cum_incidD))+
#                     theme(legend.position = "none")+
#                     #coord_cartesian(ylim = c(0, 3000))+
#                     labs(x="Date",y="Number of Deaths")+
#                     facet_wrap(~USPS),
#                   final%>%
#                     filter(USPS==abr)%>%
#                     ggplot()+geom_col(aes(x=date,y=incidD_new))+
#                     theme(legend.position = "none")+
#                     labs(title="CORRECTED",x="Date",y="Number of Deaths")+
#                     #coord_cartesian(ylim = c(0, 3000))+
#                     facet_wrap(~USPS),
#                   final%>%
#                     filter(USPS==abr)%>%
#                     ggplot()+geom_col(aes(x=date,y=cum_incidD_new))+
#                     theme(legend.position = "none")+
#                     labs(x="Date",y="Number of Deaths")+
#                     #coord_cartesian(ylim = c(0, 3000))+
#                     facet_wrap(~USPS),
#                   ncol = 1, nrow = 4))
#   
# }
# 
# 
# 
# dev.off()
# 
# # WEIGHTED SPLINE Iterative One one outlier-------------------------------------
# info <- dat %>%
#   select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled)
# 
# # temp<-info %>% 
# #   filter(incidD>0)%>%
# #   group_by(state)%>%
# #   summarise(low=median(incidD)-2.3*IQR(incidD))
# 
# info<-info %>%
#   group_by(USPS) %>%
#   mutate(cum_incidD=cumsum(incidD),cum_spline_val=spline_function(date,cum_incidD))
# 
# pdf(file= "WEIGHTED_SPLINE_RATIO_METHOD.pdf" )
# for (abr in  c(state.abb, "PR", "DC")){
#   #abr="AL"
#   info1<-info %>%
#     #group_by(USPS) %>% 2022-03-19
#     filter(USPS==abr)
#   
#   out<-info1 %>% filter(pois_stat_99.9_carlinga_scaled==1) %>% pull(date)
#   out<-out[length(out)]
#   if(length(out)>0){
#   info1<-info1 %>%
#     mutate(spline_weight=ifelse(date >= out,1,0.001)) %>%
#     mutate(spline_weighted=spline_function(date=date,incidD=cum_incidD,weights=spline_weight))
#   }else{
#     info1<-info1 %>%
#       mutate(spline_weighted=cum_spline_val)
#     }
#   
#  
# print(info1%>%
#   ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   geom_line(aes(x=date,y=cum_spline_val),color="black")+
#   geom_line(aes(x=date,y=spline_weighted),color="blue")+
#   labs(title=abr)+
#   theme(legend.position = "none"))
#   #coord_cartesian(ylim = c(0, 3000))+
#   #facet_wrap(~USPS))
# }
# 
# dev.off()
# 
# info1<- info1 %>%
#   group_by(USPS) %>%
#   mutate(incidD_new=cum_diff(spline_weighted),cum_incidD_new=cumsum(incidD_new))
# 
# ggarrange(info1 %>%
#             ggplot()+geom_col(aes(x=date,y=incidD))+
#             theme(legend.position = "none")+
#             labs(title="INCID OG",x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           info1 %>%
#             ggplot()+geom_col(aes(x=date,y=cum_incidD))+
#             theme(legend.position = "none")+
#             labs(title="CUM OG",x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           info1 %>%
#             ggplot()+geom_col(aes(x=date,y=incidD_new))+
#             theme(legend.position = "none")+
#             labs(title="INCID NEW",x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           info1 %>%
#             ggplot()+geom_col(aes(x=date,y=spline_weighted))+
#             theme(legend.position = "none")+
#             labs(title="CUM NEW",x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           ncol = 1, nrow = 4)
# 
# 
# # Single Pass ------------------------------------------------------------------
# 
# #spline_distance_weighted <- function(incidD,pois_stat_99.9_carlinga_scaled){
# 
# # info2 <- data.frame(incidD,pois_stat_99.9_carlinga_scaled)
# 
# info2<-dat %>%
#   select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled)
# 
# info2<-info2 %>%
#   filter(USPS=="MD") %>%
#   #group_by(USPS) %>%
#   mutate(cum_incidD=cumsum(incidD),week_diff=rep(0,n()),div=rep(1,n()))
# 
# 
# pos=nrow(info2)
# print(info2$pois_stat_99.9_carlinga_scaled[pos])
# while(info2$pois_stat_99.9_carlinga_scaled[pos]==0){
#   pos=pos-1
# }
# posit=pos
# while(posit>0){
# posit=posit-1
# wd=1
# div_posit<-c()
# 
# while(info2$pois_stat_99.9_carlinga_scaled[posit]==0){
#   info2$week_diff[posit]=wd
#   div_posit<-c(div_posit,posit)
#   posit=posit-1
#   wd=wd+1
#   if(posit==0){break}
# }
# info2$div[div_posit]=wd-1
# }
# 
# info2<-info2 %>% 
#   mutate(weight=(week_diff-1)/div,
#          weight=abs(weight),
#          weight=ifelse(weight==1,100,weight),
#          weight=ifelse(weight==0,0.000000001,weight),
#          cum_spline_val=spline_function(date=date,incidD=cum_incidD),
#          cum_spline_distance_weighted=spline_function(date=date,incidD=cum_incidD,
#                                                       weights=weight))
# 
# # return(info2 %>% pull(cum_spline_distance_weighted))
# # 
# # }
# 
# # dat %>%
# #   select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled) %>%
# #   group_by(USPS)%>%
# #   mutate(spline_distance_weighted=spline_distance_weighted(incidD=incidD,
# #                                                            pois_stat_99.9_carlinga_scaled=pois_stat_99.9_carlinga_scaled))
# 
# 
# info2<- info2 %>%
#   mutate(incidD_new=diff(c(0, cum_spline_distance_weighted)))
# 
# 
# ggarrange(info2 %>%
#             ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_val),color="black",size=1)+
#             #geom_line(aes(x=date,y=cum_spline_distance_weighted),color="blue",size=1) +
#             theme(legend.position = "none")+
#             labs(x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           info2 %>%
#             ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_val),color="black",size=1)+
#             geom_line(aes(x=date,y=cum_spline_distance_weighted),color="green",size=1) +
#             theme(legend.position = "none")+
#             labs(x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           info2 %>%
#             ggplot()+geom_col(aes(x=date,y=incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_val),color="black",size=1)+
#             #geom_line(aes(x=date,y=cum_spline_distance_weighted),color="blue",size=1) +
#             theme(legend.position = "none")+
#             labs(x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           info2 %>%
#             ggplot()+geom_col(aes(x=date,y=cum_spline_distance_weighted,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_val),color="black",size=1)+
#             #geom_line(aes(x=date,y=cum_spline_distance_weighted),color="blue",size=1) +
#             theme(legend.position = "none")+
#             labs(x="Date",y="Number of Deaths")+
#             #coord_cartesian(ylim = c(0, 3000))+
#             facet_wrap(~USPS),
#           ncol=1,nrow=4)
# 
# 
# View(info2 %>% select(date,USPS,incidD,pois_stat_99.9_carlinga_scaled,week_diff,div,weight))
# 
# # Spline with ideal spar -------------------------------------------------------
# 
# spline_function_spar<- function(date,incidD,weights=rep(1,length(incidD)),#knots=50,
#                            all_knots=TRUE,spar=NULL){ #NOTE IncidD has to be cumlative)
#   # date_n<-date[incidD>0]
#   # incidD_n<-incidD[incidD>0]
#   # weights_n<-weights[incidD>0]
#   mod.ss <- ss(date,incidD, w= weights,all.knots=all_knots,spar=spar)
#   # nknots = knots
#   # View(data.frame(mod.ss$y))
#   # return(c(incidD[incidD<=0],mod.ss$y))
#   return(mod.ss)
# }
# 
# spline_function_lambda<- function(date,incidD,weights=rep(1,length(incidD)),#knots=50,
#                                 all_knots=TRUE,lambda=0.0000006){ #NOTE IncidD has to be cumlative)
#   # date_n<-date[incidD>0]
#   # incidD_n<-incidD[incidD>0]
#   # weights_n<-weights[incidD>0]
#   mod.ss <- ss(date,incidD, w= weights,all.knots=all_knots,lambda=lambda)
#   # nknots = knots
#   # View(data.frame(mod.ss$y))
#   # return(c(incidD[incidD<=0],mod.ss$y))
#   return(mod.ss)
# }
# 
# # temp<-info2 %>%
# #   mutate(cum_spline_new=round(spline_function_new(date=date,incidD=cum_incidD,weights=weight,spar=0)))
# 
# weight_function<-function(incidD,pois_stat_99.9_carlinga_scaled){
#   info2<-data.frame(incidD,pois_stat_99.9_carlinga_scaled)
#   info2<-info2 %>%
#     #filter(USPS=="AL") %>%
#     #group_by(USPS) %>%
#     mutate(cum_incidD=cumsum(incidD),week_diff=rep(0,n()),div=rep(1,n()))
#   
#   pos=nrow(info2)
#   while(info2$pois_stat_99.9_carlinga_scaled[pos]==0){
#     pos=pos-1
#     if(pos<1){break}
#   }
#   posit=pos
#   while(posit>0){
#     posit=posit-1
#     wd=1
#     div_posit<-c()
#     
#     while(info2$pois_stat_99.9_carlinga_scaled[posit]==0){
#       info2$week_diff[posit]=wd
#       div_posit<-c(div_posit,posit)
#       posit=posit-1
#       wd=wd+1
#       if(posit==0){break}
#     }
#     info2$div[div_posit]=wd-1
#   }
#   
#   info2<-info2 %>% 
#     mutate(weight=(week_diff-1)/div,
#            weight=abs(weight),
#            weight=ifelse(weight==1,1000,weight),
#            weight=ifelse(weight==0,0.000000001,weight))
#   
#   return(info2$weight)
# }
# 
# 
# info2<-dat %>%
#   select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled)
#  
# info2<-info2 %>%
#   group_by(USPS) %>%
#   mutate(weight=weight_function(incidD = incidD,
#                                 pois_stat_99.9_carlinga_scaled = pois_stat_99.9_carlinga_scaled),
#          cum_incidD=cumsum(incidD)#,
#          #cum_spline_spar=round(spline_function_lambda(date=date,incidD=cum_incidD,weights = weight,lambda=1.00e-09)$y),
#          #cum_spline_spar=ifelse(cum_spline_spar<0,0,cum_spline_spar),
#          #incidD_new=diff(c(0, cum_spline_spar))
#          )
# 
# spline_function_lambda<- function(date,cum_incidD,weights=rep(1,length(incidD)),#knots=50,
#                                   all_knots=TRUE,lambda=0.0000006){ 
#   mod.ss <- ss(date,cum_incidD, w= weights,all.knots=all_knots,lambda=lambda)
#   return(mod.ss)
# }
# 
# temp<- info2 %>% filter(USPS=="NY")
# if(nrow(temp %>% filter(pois_stat_99.9_carlinga_scaled==1))==0){
#   ideal=0
#   print("ENTER CONTINUE STATEMENT SINCE NO OUTLIER PRESENT")
# }
# ideal=6.00e-07
# for(l in seq(6.00e-07,1.00e-09,-1.00e-09)){
#   temp<- temp %>%
#     mutate(cum_spline=round(spline_function_lambda(date=date,incidD=cum_incidD,weights = weight,lambda=l)$y),
#            cum_spline=ifelse(cum_spline<0,0,cum_spline))
#   bool<-temp %>%
#     filter(pois_stat_99.9_carlinga_scaled==1) %>%
#     mutate(bol=cum_incidD>cum_spline) %>%
#     pull(bol)
#   bool<-bool[1]
#   bool
#   if(bool){
#     ideal=l+1.00e-09
#     break
#   }
# }
# temp<- temp %>%
#   mutate(cum_spline=round(spline_function_lambda(date=date,incidD=cum_incidD,weights = weight,lambda=ideal)$y),
#          cum_spline=ifelse(cum_spline<0,0,cum_spline),
#          incidD_new=diff(c(0, cum_spline)))
# 
# incid_max= c(temp %>% pull(incidD),
#              temp %>% pull(incidD_new))
# incid_max=max(incid_max)
# cum_max= c(temp %>% pull(cum_incidD),
#            temp %>% pull(cum_spline))
# cum_max=max(cum_max)
# 
# ggarrange(temp %>%
#             ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#             theme(legend.position = "none")+
#             labs(title=paste("NY"," ORIGINAL"))+
#             coord_cartesian(ylim = c(0, incid_max)),
#           temp %>%
#             ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             geom_line(aes(x=date,y=cum_spline),color="blue",size=1) +
#             theme(legend.position = "none")+
#             coord_cartesian(ylim = c(0, cum_max)),
#           temp %>%
#             ggplot()+geom_col(aes(x=date,y=incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#             theme(legend.position = "none")+
#             labs(title="NEW")+
#             coord_cartesian(ylim = c(0, incid_max)),
#           temp %>%
#             ggplot()+geom_col(aes(x=date,y=cum_spline,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#             theme(legend.position = "none")+
#             coord_cartesian(ylim = c(0, cum_max)),
#           ncol=1,nrow=4
# )
# 
# 
# temp<-info2 %>%
#   filter(USPS=="NY")
# 
# FL-LambdaIterative_Weights1_10_1000
# 
# 
# 
# bool<-info2 %>%
#   filter(USPS==abr,pois_stat_99.9_carlinga_scaled==1) %>%
#   mutate(bol=cum_incidD<=cum_spline_spar) %>%
#   pull(bol)
# bool
# bool<-bool[1]
# bool
# 
# if(bool)  
# mod.ss <- ss(date,incidD, w= weights,all.knots=all_knots,lambda=lambda)
# 
# View(temp)
# 
# # info2<-info2 %>%
# #   rename(weight_0to1_10=weight, 
# #          cum_incidD_0to1_10=cum_incidD,
# #          cum_spline_spar_0to1_10=cum_spline_spar)
# 
# abr="FL"
# 
# ggarrange(info2 %>%
#   filter(USPS==abr)%>%
#   ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   #geom_line(aes(x=date,y=cum_spline_spar),color="blue") +
#   theme(legend.position = "none")+
#   facet_wrap(~USPS),
# info2 %>%
#   filter(USPS==abr)%>%
#   ggplot()+geom_col(aes(x=date,y=incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   #geom_line(aes(x=date,y=cum_spline_spar),color="blue") +
#   theme(legend.position = "none")+
#   facet_wrap(~USPS),ncol=1,nrow=2)
# 
# 
# 
# 
# 
# 
# 
# # pdf(file= "WEIGHTED_SPLINE_0to1_50.pdf" )
# # for (abr in  c("AK","FL","MD","NY")){
# # print(info2 %>%
# #   filter(USPS==abr)%>%
# #   ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
# #   geom_line(aes(x=date,y=cum_spline_spar),color="blue") +
# #   theme(legend.position = "none")+
# #   facet_wrap(~USPS))
# #   
# # }
# # dev.off()
# 
# #pdf(file= "WEIGHTED_SPLINE_RATIO_SPAR0_REPLACE_NEGATIVE_METHOD.pdf" )
# #for (abr in  c(state.abb, "PR", "DC")){
#   incid_max= c(info2 %>% filter(USPS==abr) %>% pull(incidD),
#                info2 %>% filter(USPS==abr) %>% pull(incidD_new))
#   incid_max=max(incid_max)
#   cum_max= c(info2 %>% filter(USPS==abr) %>% pull(cum_incidD),
#                info2 %>% filter(USPS==abr) %>% pull(cum_spline_spar))
#   cum_max=max(cum_max)
#  # print(
#     ggarrange(info2 %>%
#                     filter(USPS==abr) %>%
#                     ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#                     #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#                     theme(legend.position = "none")+
#                     labs(title=paste(abr," ORIGINAL"))+
#                     coord_cartesian(ylim = c(0, incid_max)),
#                   info2 %>%
#                     filter(USPS==abr) %>%
#                     ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#                     geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#                     theme(legend.position = "none")+
#                     coord_cartesian(ylim = c(0, cum_max)),
#                   info2 %>%
#                     filter(USPS==abr) %>%
#                     ggplot()+geom_col(aes(x=date,y=incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#                     #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#                     theme(legend.position = "none")+
#                     labs(title="NEW")+
#                     coord_cartesian(ylim = c(0, incid_max)),
#                   info2 %>%
#                     filter(USPS==abr) %>%
#                     ggplot()+geom_col(aes(x=date,y=cum_spline_spar,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#                     #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#                     theme(legend.position = "none")+
#                     coord_cartesian(ylim = c(0, cum_max)),
#                   ncol=1,nrow=4
# )##)
#     View(info2 %>%
#            filter(USPS==abr)) 
# #}
# 
# temp<-info2 %>%
#   #filter(USPS=="AL") %>%
#   group_by(USPS) %>%
#   summarize(direction=ifelse(all(cum_spline_spar==sort(cum_spline_spar)),"MONOTONIC","NON-MONOTONIC"))
# 
# dev.off()
# 
# temp<- info2 %>%
#   filter(USPS=="MD") %>%
#   mutate(cum_spline_lambda=round(spline_function_lambda(date=date,incidD=cum_incidD,weights = weight,lambda =0.000000005)$y))
# 
# temp%>%
#   ggplot()+geom_col(aes(x=date,y=cum_incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#   #geom_line(aes(x=date,y=cum_spline_spar),color="blue",size=1) +
#   geom_line(aes(x=date,y=cum_spline_lambda),color="black",size=1) +
#   theme(legend.position = "none")
# 
# ifelse(all(temp$cum_spline_lambda==sort(temp$cum_spline_lambda)),"MONOTONIC","NON-MONOTONIC")
# #temp$cum_spline_lambda==sort(temp$cum_spline_lambda)
# 
# # info2<- info2 %>%
# #   filter(USPS=="FL") 
# # 
# # temp<-ss(info2$date,info2$incidD, w= info2$weights,all.knots=all_knots,spar=0,xmax=(info2$date[info2$incidD>0])[1])
# 
# 
# 
# # INCIDENCE SPLINE BACK DISTRIBUTION -------------------------------------------
# spline_function_lambda<- function(date,incidD,weights=rep(1,length(incidD)),#knots=50,
#                                   all_knots=TRUE,lambda=0.0000006){ 
#   mod.ss <- ss(date,incidD, w= weights,all.knots=all_knots,lambda=lambda)
#   return(mod.ss)
# }
# 
# info2<-dat %>%
#   select(incidD,state,USPS,date,pois_stat_99.9_carlinga_scaled) %>%
#   mutate(weight=ifelse(pois_stat_99.9_carlinga_scaled==0,1,0.0000000001))
# 
# info2<- info2 %>%
#   #filter(pois_stat_99.9_carlinga_scaled==0) %>%
#   group_by(USPS) %>%
#   mutate(spline_lambda=round((ss(date,incidD,lambda =0,w=weight))$y),
#          spline_spar=round((ss(date,incidD,spar =0,w=weight))$y))
#          
# # 
# # info2<- left_join(info2,info2_no,by=c("date","USPS","incidD","state","pois_stat_99.9_carlinga_scaled"))
# 
# if(FALSE){
# pdf(file= "INCIDENT_DEATHS_ALL_WITH_NEGLIGIBLE_OUTLIER_WEIGHT_spar_0.pdf" )
# for(abr in  c(state.abb, "PR", "DC")){
#   print(info2 %>%
#           filter(USPS==abr) %>%
#           ggplot()+ geom_col(aes(x=date,incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#           geom_line(aes(x=date,y=spline),color="blue")+
#           labs(title=abr)+
#           theme(legend.position = "none"))
# }
# dev.off()
# }
# 
# 
# wave_number<-function(date,incidD,spline_spar){
#   temp<- data.frame(date,incidD) %>%
#     mutate(slp=c(0,diff(spline_spar,lag=2),0),
#            wave_number=rep(1,length(incidD)))
#   wave=1
#   for (i in 1:(nrow(temp)-1)){
#     temp$wave_number[i]<-wave
#     if(temp$slp[i]<0 & temp$slp[i+1]>=0){
#       wave=wave+1
#     }
#   }
#   temp$wave_number[i+1]<-wave-1
#   return(temp$wave_number)
# }
# 
# info2<- info2 %>%
#   group_by(USPS) %>%
#   mutate(wave=wave_number(date,incidD,spline_spar))
# 
# abr="NY"
# if(FALSE){
# pdf(file= "INCIDENT_DEATHS_AND_WAVES.pdf" )
# #for(abr in  c("AK","FL","MD","NY")){
# for(abr in c(state.abb, "PR", "DC")){
# temp <- info2 %>%
#   filter(USPS==abr)
# 
# wave=1
# 
# for (i in 1:(nrow(temp)-1)){
#   temp$wave_number[i]<-wave
#   if(temp$slp[i]<0 & temp$slp[i+1]>=0){
#     wave=wave+1
#   }
# }
# temp$wave_number[i+1]<-wave-1
# 
# 
# print(
#     temp %>%
#       filter(USPS==abr) %>%
#       ggplot()+ geom_col(aes(x=date,incidD,fill=factor(wave_number)))+
#       geom_line(aes(x=date,y=spline_spar),color="blue")+
#       labs(title=abr)+
#       theme(legend.position = "none")
# )
# }
# dev.off()
# }
# 
# 
# # SPLINE PREDICT ---------------------------------------------------------------
# temp <- info2 %>%
#   filter(USPS=="")
# 
# spline_omit_out<-function(date,incidD,pois_out){
# # date<-temp %>% pull(date)
# # incidD<-temp %>% pull(incidD)
# # pois_out<-temp %>% pull(pois_stat_99.9_carlinga_scaled)
# 
# #out<-date[pois_out==1]
# # out<- temp %>% 
# #   filter(pois_stat_99.9_carlinga_scaled==1) %>%
# #   pull(date)
#   
# if(any(pois_out==1)){
# incidD[pois_out==1]<-round((predict(ss(date[pois_out!=1],incidD[pois_out!=1],lambda =0),
#                                      x=date[pois_out==1]))$y)
# }
# return(incidD)
# }
# 
# info2<- info2 %>%
#   group_by(USPS) %>%
#   mutate(spline_omi=spline_omit_out(date=date,
#                                     incidD=incidD,
#                                     pois_out=pois_stat_99.9_carlinga_scaled),
#          spline_omi=ifelse(spline_omi<0,0,spline_omi))
# 
# abr="FL"
# if(FALSE){
#   pdf(file= "INCIDENT_DEATHS_OMITTING_OUTLIERS.pdf" )
#   for(abr in  c(state.abb, "PR", "DC")){
#     print(info2 %>%
#             filter(USPS==abr) %>%
#             ggplot()+ geom_col(aes(x=date,incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             geom_line(aes(x=date,y=spline_lambda),color="black")+
#             geom_line(aes(x=date,y=spline_omi),color="green")+
#             labs(title=abr)+
#             theme(legend.position = "none"))
#   }
#   dev.off()
# }
# 
# 
# spline_omi_distri<-function(date,incidD,spline_omi,pois_out,wave){
#   temp<-data.frame(date,incidD,spline_omi,pois_out,wave)
#   temp<- temp %>%
#     mutate(diff=incidD-spline_omi)
#   
#   out<-temp %>%
#     filter(pois_out==1) %>%
#     pull(date)
#   
#   wavelength<-temp %>%
#     group_by(wave) %>%
#     summarise(wlength=length(wave))
#   
#   for(i in out){
#     #i=out[1]
#     w<- temp %>%
#       filter(date==i) %>%
#       pull(wave)
#     
#     wave_pos<- nrow(temp %>%
#                       filter(wave==w) %>%
#                       filter(date<=i))
#     
#     if(w!=1 & wave_pos< (wavelength %>% filter(wave==w) %>% pull(wlength))/4){
#       w=c(w-1,w)
#     }
#     
#     temp<-temp %>%
#       mutate(flag=rep(0,length(date)),
#              flag=ifelse(wave %in% w & date<=i,1,flag),
#              incidD_new=incidD*flag,
#              diff_new=diff*flag,
#              spline_omi_new=spline_omi*flag,
#              incidD_add=round(spline_omi_new/sum(spline_omi_new)*max(diff_new)),
#              incidD_new=incidD_add+spline_omi_new,
#              # incidD_new=(round(spline_omi_new/sum(spline_omi_new))*max(diff_new))+
#              #   spline_omi_new),
#              incidD=ifelse(flag==1,incidD_new,incidD))
#   }
#   
#   return(temp %>% pull(incidD))
#   
# }
# 
# temp2<- info2 %>%
#   group_by(USPS) %>%
#   mutate(incidD_new=spline_omi_distri(date,incidD,spline_omi,
#                                       pois_stat_99.9_carlinga_scaled,wave))
# 
# pdf(file= "INCIDENT_DEATHS_CORRECTED.pdf" )
# for(abr in  c(state.abb, "PR", "DC")){
# print(ggarrange(temp2 %>%
#             filter(USPS==abr) %>%
#             ggplot()+ geom_col(aes(x=date,incidD,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             geom_col(aes(x=date,incidD_new,fill="blue"))+
#             #geom_line(aes(x=date,y=spline_lambda),color="black")+
#             #geom_line(aes(x=date,y=spline_omi),color="green")+
#             labs(title=abr)+
#             theme(legend.position = "none"),
#           temp2 %>%
#             filter(USPS==abr) %>%
#             ggplot()+ geom_col(aes(x=date,incidD_new,fill=factor(pois_stat_99.9_carlinga_scaled)))+
#             #geom_line(aes(x=date,y=spline_lambda),color="black")+
#             #geom_line(aes(x=date,y=spline_omi),color="green")+
#             labs(title=abr)+
#             theme(legend.position = "none"),nrow = 1,ncol = 1))
#             
# }
# dev.off()
# 
# 
# final<- temp2 %>%
#   select(date,incidD_new,USPS) %>%
#   mutate(TYPE="new") %>%
#   rename(incidD=incidD_new) %>%
#   bind_rows(temp2 %>%
#               select(date,incidD,USPS) %>%
#               mutate(TYPE="old"))
# 
# pdf(file= "INCIDENT_DEATHS_CORRECTED_OVERLAPPED.pdf" )
# for(abr in  c(state.abb, "PR", "DC")){
# print(final %>%
#   filter(USPS==abr) %>%
#   ggplot()+ geom_col(aes(x=date,incidD,color=TYPE,fill=TYPE),position = "identity")+
#   #geom_line(aes(x=date,y=spline_lambda),color="black")+
#   #geom_line(aes(x=date,y=spline_omi),color="green")+
#   labs(title=abr))
# }
# dev.off()
# 
# date<- temp2$date
# incidD<- temp2$incidD
# spline_omi<- temp2$spline_omi
# pois_out<- temp2$pois_stat_99.9_carlinga_scaled
# wave<-temp2$wave
# 
# 
# bol<- date>=((date[incidD!=0])[1])
# date<- date[bol]
# incidD<- incidD[bol]
# spline_omi<- spline_omi[bol]
# pois_out<- pois_out[bol]
# wave<-wave[bol]
# omi_diff<- incidD-spline_omi
#   
# temp<-temp %>%
#   filter(date>=(temp %>%
#   filter(incidD!=0) %>%
#   pull(date))[1])
# 
# # wavelength<- temp %>%
# #   group_by(wave)%>%
# #   summarise(wavelength=length(wave))
# 
# out<-temp %>%
#   filter(pois_stat_99.9_carlinga_scaled==1) %>%
#   pull(date)
# 
# out<- date[pois_out==1]
# 
# for(i in out){
#   i=out[1]
#   w=temp %>%
#     filter(date==i) %>%
#     pull(wave)
#   w= wave[date==i]
#   
#   wavelength<- nrow(temp %>% filter(wave==w))
#   wavelength<- sum(wave==w)
#   out_occur<-grep(i,temp$date)
#   out_occur<-grep(i,date)
#   if(out_occur<round(wavelength/4) & w!=1){
#     w=c(w-1,w)
#   }
#   temp2 <- temp %>%
#     filter(wave %in% w) %>%
#     filter(date<=i)
#   
#   bol= wave %in% w & date<=i
#   
#   val <- sum(temp2 %>% pull(omi_diff))
#   val <- (omi_diff[bol])[sum(bol)]
#   temp3 <- temp2 %>%
#     mutate(incidD_new=round(spline_omi/sum(spline_omi)*sum(omi_diff)),
#            incidD_new=spline_omi+incidD_new)
# }
# 
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------
# 
# datD<-read_csv("data/CSSE_WeeklyDeaths_incl_assigned.csv")
# datI<-read_csv("data/CSSE_WeeklyCases_incl_assigned.csv")
# 
# datD<- datD %>%
#   mutate(incidD =ifelse(incidD<0,0,incidD))
# 
# datI<- datI %>%
#   mutate(incidI =ifelse(incidI<0,0,incidI))
# 
# 
# final<-rbind(datI %>% select(incid=incidI,date,USPS) %>% mutate(type="Cases"),
#              datD %>% select(incid=incidD,date,USPS) %>% mutate(type="Deaths"))
# ci=99.9
# p=1-(1-ci/100)/2
# 
# datD<-datD %>%
#   group_by(USPS) %>%
#   mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0),
#          mov_incidD=moving_average(incidD,3),
#          pois_out_statology = as.numeric(incidD > 0.5*qchisq(p= p, df=2*(mov_incidD +1))),
#          incidD_movavg_simple=ifelse(incidD>2*mov_incidD,1,0),
#          pois_stat_99.9_carlinga_scaled=pois_out_statology*carlinga_out*incidD_movavg_simple) %>%
#   select(incidD,date,USPS,pois_stat_99.9_carlinga_scaled) %>%
#   mutate(weight=ifelse(pois_stat_99.9_carlinga_scaled==0,1,0.0000000001)) %>%
#   mutate(spline_spar=round((ss(date,incidD,spar =0,w=weight))$y),
#          wave=wave_number(date,incidD,spline_spar))
# 
# datI<-datI %>%
#   select(incidI,date,USPS) %>%
#   group_by(USPS) %>%
#   mutate(spline_spar=round((ss(date,incidI,spar =0))$y),
#          wave=wave_number(date,incidI,spline_spar))
# 
# pdf(file= "INCIDENT_DEATHS_AND_CASES_WITH_WAVES.pdf" )
# for(abr in  c(state.abb, "PR", "DC")){
# print(ggarrange(datI %>%
#   filter(USPS==abr) %>%
#   ggplot() +geom_col(aes(x=date,y=incidI,fill=factor(wave)))+
#     labs(title=abr),
# datD %>%
#   filter(USPS==abr) %>%
#   ggplot() +geom_col(aes(x=date,y=incidD,fill=factor(wave))),ncol=1,nrow=2))
# }
# dev.off()
# 
# p<-ggplot(datI %>% filter(USPS=="NY"),aes(date,incidI))+
#   geom_col(color="orange",fill="orange") 
# 
# 
# p+geom_col(aes(y=(datD %>% filter(USPS=="NY") %>% pull(incidD))*10),color="blue",fill="blue")
# # +
# #   scale_y_continuous(sec.axis = sec_axis(~./100000, name = "incidD"))
# 
# final %>%
#   filter(USPS==abr) %>%
#   #filter(type=="Deaths") %>%
#   ggplot() + geom_col(aes(x=date,y=incid,fill=type),position="jitter")
#   
# x <- 1:10
# y1 <- x^2
# y2 <- x*10
# 
# # create the first plot with y1
# p <- ggplot(data.frame(x, y1), aes(x, y1)) + 
#   geom_line(color = "blue", size = 1)
# 
# # add the second plot with y2 on a different scale
# p + geom_line(aes(y = y2), color = "red", size = 1) +
#   scale_y_continuous(sec.axis = sec_axis(~./10, name = "y2"))
# 
# 
