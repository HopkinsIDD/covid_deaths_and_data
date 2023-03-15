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

# dat<-read_csv("data/CSSE_WeeklyDeaths_incl_assigned.csv")
# #dat<-read_csv("data/CDC_CSSE_WeeklyDeaths.csv")
# dat<-dat%>%
#   #filter(source=='csse')%>%
#   select(incidD,state,date,USPS) %>% 
#   mutate(incidD=ifelse(incidD<0,0,incidD))

datD<-read_csv("data/CSSE_WeeklyDeaths_incl_assigned.csv")
datI<-read_csv("data/CSSE_WeeklyCases_incl_assigned.csv")

datD<- datD %>%
  select(incidD,date,USPS) %>%
  mutate(incidD =ifelse(incidD<0,0,incidD))

datI<- datI %>%
  select(incidI,date,USPS) %>%
  mutate(incidI =ifelse(incidI<0,0,incidI))

dat<- datD %>%
  left_join(datI,by=c("date","USPS"))

moving_average<- function(data,w=3){
  mov_avg<-rep(0,length(data))
  for(i in ((w+1)/2) : (length(data) - (w-1)/2)){
    mov_avg[i]=(mean(data[(i-(w-1)/2):(i+(w-1)/2)]))
  }
  return(mov_avg)
}

wave_number<-function(date,incidD,spline_spar){
  temp<- data.frame(date,incidD) %>%
    mutate(slp=c(0,diff(spline_spar,lag=2),0),
           wave_number=rep(1,length(incidD)))
  wave=1
  for (i in 1:(nrow(temp)-1)){
    temp$wave_number[i]<-wave
    if(temp$slp[i]<0 & temp$slp[i+1]>=0){
      wave=wave+1
    }
  }
  temp$wave_number[i+1]<-wave-1
  return(temp$wave_number)
}

spline_omit_out<-function(date,incidD,pois_out){
  if(any(pois_out==1)){
    incidD[pois_out==1]<-round((predict(ss(date[pois_out!=1],incidD[pois_out!=1],lambda =0),
                                        x=date[pois_out==1]))$y)
  }
  return(incidD)
}

calc_zero_length<-function(incidD,date,d){
  # incidD<-temp$incidD
  # date<-temp$date
  # d=i
  zero_length=0
  for (j in (length(date[date<=d])-1):1){
    if(incidD[j]!=0){
      break
    }
  zero_length=zero_length+1
  }
  return(zero_length)
}

spline_omi_distri_deaths<-function(date,incidD,spline_omi,pois_out,wave){
  temp<-data.frame(date,incidD,spline_omi,pois_out,wave)
  temp<- temp %>%
    mutate(diff=incidD-spline_omi)
  out<-temp %>%
    #filter(pois_stat_99.9_carlinga_scaled==1)%>%
    filter(pois_out==1) %>%
    pull(date)
  
  wavelength<-temp %>%
    group_by(wave) %>%
    summarise(wlength=length(wave))
  
  for(i in out){
    #i=out[4]
    w<- temp %>%
      filter(date==i) %>%
      pull(wave)
    
    wave_pos<- nrow(temp %>%
                      filter(wave==w) %>%
                      filter(date<=i))
    dist_length=wave_pos
    if(w!=1 & wave_pos< (wavelength %>% filter(wave==w) %>% pull(wlength))/4){
      w=c(w-1,w)
      dist_length=dist_length +  wavelength %>% filter(wave==w[1]) %>% pull(wlength)
    }
    zero_length<-calc_zero_length(temp$incidD,temp$date,d=i)
    date_l<-(temp %>% filter(wave == w[1]) %>% pull(date))[1]
    if((zero_length+1)>dist_length){
      date_l=(temp %>% filter(date<=i) %>% pull(date))
      date_l=date_l[length(date_l)- zero_length]
    }
    
    temp<-temp %>%
      mutate(flag=rep(0,length(date)),
             #flag=ifelse(wave %in% w & date<=i,1,flag),
             flag=ifelse(date>=date_l & date<=i,1,flag),
             incidD_new=incidD*flag,
             diff_new=diff*flag,
             spline_omi_new=spline_omi*flag,
             incidI_new=incidI*flag,
             # incidD_add=ifelse(bol,
             #                   round(spline_omi_new/sum(spline_omi_new)*max(diff_new)),
             #                   round(incidI_new/sum(incidI_new)*max(diff_new))),
             # incidD_new=incidD_add+spline_omi_new,
             incidD_new=round(spline_omi_new/sum(spline_omi_new)*max(diff_new)+spline_omi_new),
             incidD=ifelse(flag==1,incidD_new,incidD))
  }
  return(temp %>% pull(incidD))
}

spline_omi_distri_cases<-function(date,incidD,spline_omi,pois_out,wave,incidI){
  temp<-data.frame(date,incidD,spline_omi,pois_out,wave,incidI)
  # temp<- temp %>%
  #   mutate(diff=incidD-spline_omi)
  out<-temp %>%
    #filter(pois_stat_99.9_carlinga_scaled==1)%>%
    filter(pois_out==1) %>%
    pull(date)
  
  wavelength<-temp %>%
    group_by(wave) %>%
    summarise(wlength=length(wave))
  
  for(i in out){
    #i=out[2]
    w<- temp %>%
      filter(date==i) %>%
      pull(wave)
    
    wave_pos<- nrow(temp %>%
                      filter(wave==w) %>%
                      filter(date<=i))
    dist_length=wave_pos
    if(w!=1 & wave_pos< (wavelength %>% filter(wave==w) %>% pull(wlength))/4){
      w=c(w-1,w)
      dist_length=dist_length +  wavelength %>% filter(wave==w[1]) %>% pull(wlength)
    }
    zero_length<-calc_zero_length(temp$incidD,temp$date,d=i)
    date_l<-(temp %>% filter(wave == w[1]) %>% pull(date))[1]
    if((zero_length+1)>dist_length){
      date_l=(temp %>% filter(date<=i) %>% pull(date))
      date_l=date_l[length(date_l)- zero_length]
    }
    
    temp<-temp %>%
      mutate(diff=incidD-spline_omi,
             flag=rep(0,length(date)),
             #flag=ifelse(wave %in% w & date<=i,1,flag),
             flag=ifelse(date>=date_l & date<=i,1,flag),
             incidD_new=incidD*flag,
             diff_new=diff*flag,
             spline_omi_new=spline_omi*flag,
             incidI_new=incidI*flag,
             # incidD_add=ifelse(bol,
             #                   round(spline_omi_new/sum(spline_omi_new)*max(diff_new)),
             #                   round(incidI_new/sum(incidI_new)*max(diff_new))),
             # incidD_new=incidD_add+spline_omi_new,
             incidD_add=incidI_new/sum(incidI_new)*max(diff_new),
             incidD_new=round(incidD-diff_new+incidD_add),
             #incidD_new=round(incidI_new/sum(incidI_new)*max(diff_new)+spline_omi_new),
             incidD=ifelse(flag==1,incidD_new,incidD))
  }
  return(temp %>% pull(incidD))
}

ci=99.9
p=1-(1-ci/100)/2


method="Cases"


dat<- datD %>%
  group_by(USPS) %>%
  mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0),
         mov_incidD=moving_average(incidD,3),
         pois_out_statology = as.numeric(incidD > 0.5*qchisq(p= p, df=2*(mov_incidD +1))),
         incidD_movavg_simple=ifelse(incidD>2*mov_incidD,1,0),
         pois_stat_99.9_carlinga_scaled=pois_out_statology*carlinga_out*incidD_movavg_simple) %>%
  select(incidD,date,USPS,pois_stat_99.9_carlinga_scaled) 

if(method=="Deaths"){
temp2<-dat %>%
  group_by(USPS) %>%
  mutate(weight=ifelse(pois_stat_99.9_carlinga_scaled==0,1,0.0000000001)) %>%
  mutate(spline_spar=round((ss(date,incidD,spar =0,w=weight))$y),
         wave=wave_number(date,incidD,spline_spar),
         spline_omi=spline_omit_out(date=date,
                                    incidD=incidD,
                                    pois_out=pois_stat_99.9_carlinga_scaled),
         spline_omi=ifelse(spline_omi<0,0,spline_omi),
         incidD_new=spline_omi_distri_deaths(date,incidD,spline_omi,
                                     pois_stat_99.9_carlinga_scaled,wave))
}
if(method=="Cases"){
  temp2<- dat %>%
    left_join(datI,by=c("USPS","date")) %>%
    group_by(USPS) %>%
    mutate(spline_spar=round((ss(date,incidI,spar =0))$y),
           wave=wave_number(date,incidI,spline_spar),
           spline_omi=spline_omit_out(date=date,
                                      incidD=incidD,
                                      pois_out=pois_stat_99.9_carlinga_scaled),
           spline_omi=ifelse(spline_omi<0,0,spline_omi),
           incidD_new=spline_omi_distri_cases(date,incidD,spline_omi,
                                               pois_stat_99.9_carlinga_scaled,wave,incidI))
}

temp2 %>%
  filter(USPS==abr) %>%
  ggplot()+geom_col(aes(x=date,y=incidI,fill=factor(wave)))+
  geom_line(aes(x=date,y=spline_spar))

final<- temp2 %>%
  select(date,incidD_new,USPS,pois_stat_99.9_carlinga_scaled) %>%
  mutate(TYPE="new") %>%
  rename(incidD=incidD_new) %>%
  bind_rows(temp2 %>%
              select(date,incidD,USPS,pois_stat_99.9_carlinga_scaled) %>%
              mutate(TYPE="old"))

pdf(file= "INCIDENT_DEATHS_CORRECTED_WITH_CASES_OVERLAPPED.pdf" )
for(abr in  c(state.abb, "PR", "DC")){
  print(final %>%
          filter(USPS==abr) %>%
          ggplot()+ geom_col(aes(x=date,incidD,color=TYPE,fill=TYPE),position = "identity")+
          #geom_line(aes(x=date,y=spline_lambda),color="black")+
          #geom_line(aes(x=date,y=spline_omi),color="green")+
          labs(title=abr))
}
dev.off()

old_total<-final %>% 
  filter(TYPE=="old") %>%
  group_by(USPS) %>%
  summarise(tot_old=sum(incidD,na.rm=TRUE))

new_total<-final %>% 
  filter(TYPE=="new") %>%
  group_by(USPS) %>%
  summarise(tot_new=sum(incidD,na.rm=TRUE))

total<- new_total %>%
  left_join(old_total,by="USPS") %>%
  mutate(diff=tot_new-tot_old)
