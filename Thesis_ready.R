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



outlier_detection<- function(date,incidD){
  ci=99.9
  prop=1-(1-ci/100)/2
  return(data.frame(date,incidD)%>%
    mutate(wave=wave_number(date,incidD)) %>%
    group_by(wave) %>%
    mutate(carlinga_out=ifelse(incidD>median(incidD)+2.3*IQR(incidD),1,0),
           mov_incidD=moving_average(incidD,3),
           pois_out_statology = as.numeric(incidD > 0.5*qchisq(p= prop, df=2*(mov_incidD +1))),
           incidD_movavg_simple=ifelse(incidD>2*mov_incidD,1,0),
           pois_stat_99.9_carlinga_scaled=pois_out_statology*carlinga_out*incidD_movavg_simple,
           pois_stat_99.9_carlinga_scaled=ifelse(is.na(pois_stat_99.9_carlinga_scaled),0,
                                                 pois_stat_99.9_carlinga_scaled)) %>%
    pull(pois_stat_99.9_carlinga_scaled))
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

death_distribution<-function(date,incidD,pois_out,use_cases=FALSE,incidI=incidD,
                             support=FALSE,diff=rep(0,length(incidD)),
                             out_csse=rep(0,length(incidD)),
                                  low_date=rep(NA,length(incidD))){
  temp<-data.frame(date,incidD,pois_out,incidI,diff,low_date,out_csse)
  if(use_cases){
    if(all(incidI==incidD)){
      stop("Please provide additional data for back distribution")
    }
  }
  # if(support){
  #   if(all(out_csse==0)){
  #     stop("Please provide additional supporting data")
  #   } else{
  #     temp<- temp %>%
  #       mutate(pois_out= ifelse(pois_out ==1 | out_csse==1,1,0)) %>%
  #       select(-c(out_csse))
  #   }
  # }
  
  temp<- temp %>%
    mutate(og=incidD,
           wave=wave_number(date,incidD),
           spline_omi=spline_omit_out(date=date,
                                      incidD=incidD,
                                      pois_out=pois_out),
           spline_omi=ifelse(spline_omi<0,0,spline_omi))
  temp<- temp %>%
    mutate(spline_omi=ifelse(diff!=0,incidD-diff,spline_omi),
           diff=incidD-spline_omi)
  out<-temp %>%
    filter(pois_out==1) %>%
    pull(date)
  
  wavelength<-temp %>%
    group_by(wave) %>%
    summarise(wlength=length(wave))
  
  for(i in out){
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
    pos=which(temp %>% filter(pois_out==1) %>% pull(date) == i)
    if(pos!=1){
      #if((temp %>% filter(pois_out==1) %>% pull(wave))[pos] ==
      if(w[1] == (temp %>% filter(pois_out==1) %>% pull(wave))[pos-1]){
        date_l=((temp %>% filter(pois_out==1) %>% pull(date))[pos-1])
        date_l=(temp %>% pull(date))[which(temp %>% pull(date) == date_l)+1]
      }}
    #if((temp %>% filter(date==i) %>% pull(low_date))!="2000-01-25"){
    if(!is.na((temp %>% filter(date==i) %>% pull(low_date)))){  
      date_l_s=temp %>% filter(date==i) %>% pull(low_date)
      if(date_l_s<(temp %>% pull(date))[1]){
        date_l_s=(temp %>% pull(date))[1]
      }
      date_l_s=(temp %>% filter(date<=date_l) %>% pull(date))[nrow(temp %>% filter(date<=date_l))]
      if(date_l_s>date_l){
        date_l=date_l_s
      }
    }
    if(all(incidD==incidI)){
      temp<-temp %>%
        mutate(flag=rep(0,length(date)),
               flag=ifelse(date>=date_l & date<=i,1,flag),
               incidD_new=incidD*flag,
               diff_new=diff*flag,
               spline_omi_new=spline_omi*flag,
               incidD_new=round(spline_omi_new/sum(spline_omi_new)*max(diff_new)+spline_omi_new),
               incidD=ifelse(flag==1,incidD_new,incidD))
    }
    else{
      temp<-temp %>%
        mutate(flag=rep(0,length(date)),
               flag=ifelse(date>=date_l & date<=i,1,flag),
               incidD_new=incidD*flag,
               diff_new=diff*flag,
               spline_omi_new=spline_omi*flag,
               incidI_new=incidI*flag,
               incidD_add=incidI_new/sum(incidI_new)*max(diff_new),
               incidD_new=round(incidD-diff_new+incidD_add),
               incidD=ifelse(flag==1,incidD_new,incidD))
    }
  }
  temp<- temp %>%
    mutate(incidD=ifelse(is.nan(incidD),og,incidD))
  return(temp %>% pull(incidD))
}


################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
datD<-read.csv("final/data/CSSE_WeeklyDeaths_DIRECT_DOWNLOAD.csv")
dat<- datD %>%
  select(incidD,date,USPS) %>%
  mutate(incidD =ifelse(incidD<0,0,incidD),
         date=as.Date(date,format="%m/%d/%Y"))%>%
  group_by(USPS) %>%
  #filter(USPS=="AK") %>%
  mutate(out=outlier_detection(date,incidD))

graphs=FALSE

if(graphs){
pdf(file= "final/outlier/Outlier Detection by Combination 3 Thesis Ready.pdf" )
for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(dat %>%
          filter(USPS==abr) %>% 
          ggplot()+geom_col(aes(x=date,y=incidD,fill=factor(out)))+
          #coord_cartesian(ylim = c(0, 3000))+
          theme(legend.position = "none")+
          labs(title=abr,x="Date",y="Deaths"))#+
  #facet_wrap(~state))
}
dev.off()
}

datS<-read.csv("final/data/CSSE_WeeklyDeaths_CSSE_Correction_Records.csv")
datS<- datS %>%
  select(date,USPS,out_csse=out,no,back_date) %>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         back_date=as.Date(back_date,format="%m/%d/%Y"),
         out_csse=ifelse(is.na(out_csse),0,out_csse),
         no=ifelse(is.na(no),0,no))

# if(support){
#   dat<-dat %>%
#     left_join(datS,by=c("date","USPS")) %>%
#     mutate(out= ifelse(out ==1 | out_csse==1,1,0)) %>%
#     select(-c(out_csse))
# } else{
#   dat <- dat %>%
#     mutate(no=rep(0,length(incidD)),
#            back_date=rep(NA,length(incidD)))
# }

datI<-read_csv("final/data/CSSE_WeeklyCasess_DIRECT_DOWNLOAD.csv")
datI<- datI %>%
  select(incidI,date,USPS) %>%
  mutate(incidI =ifelse(incidI<0,0,incidI))
# if(method=="deaths"){
#   temp2<-dat %>%
#     group_by(USPS) %>%
#     mutate(incidD_new=death_distribution(date,incidD,pois_out=out,diff=no,low_date=back_date),
#            incidD_new=ifelse(is.nan(incidD_new),incidD,incidD_new))
# } else {
#   temp2<- dat %>%
#     left_join(datI,by=c("USPS","date")) %>%
#     group_by(USPS) %>%
#     mutate(incidD_new=death_distribution(date,incidD,pois_out = out,incidI=incidI,
#                                          diff=no,low_date=back_date))
# }

dat<- dat %>%
  left_join(datI,by=c("USPS","date"))

dat<-dat %>%
  left_join(datS,by=c("date","USPS"))

temp2<- dat %>%
  group_by(USPS) %>%
  mutate(incidD_new=death_distribution(date,incidD,pois_out=out,use_cases =TRUE,incidI=incidI,
                                       support = TRUE,out_csse = out_csse,
                                       diff = no,low_date = back_date))
  


final<- temp2 %>%
  select(date,incidD_new,USPS,out) %>%
  mutate(TYPE="new") %>%
  rename(incidD=incidD_new) %>%
  bind_rows(temp2 %>%
              select(date,incidD,USPS,out) %>%
              mutate(TYPE="old"))

if(graphs){
pdf(file= "final/Deaths-Corrected-Cases-With-CSSE-Support.pdf" )
for(abr in  c(state.abb, "PR", "DC","AL_H")){
  print(
    #ggarrange(
    temp2 %>%
      filter(USPS==abr) %>%
      ggplot()+ geom_col(aes(x=date,y=incidD_new),color="black",fill="black",position = "identity")+
      geom_col(aes(x=date,y=incidD),color="green",alpha=0.00001,position = "identity")+
      labs(title=abr)
    #theme(legend.key = element_rect(fill = "white", colour = "black"))
    #theme(legend.position = "none")
    # final %>%
    #   filter(USPS==abr,TYPE=="new") %>%
    #   ggplot()+ geom_col(aes(x=date,y=incidD,fill=factor(pois_stat_99.9_carlinga_scaled)),position = "identity")+
    #   theme(legend.position = "none"),nrow = 2,ncol = 1)
  )
}
dev.off()
}

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
View(total)

temp2_t <- temp2 %>% left_join(state_names)
temp2_t$state<-factor(temp2_t$state,levels = unique(temp2_t$state),ordered=TRUE)
temp2_t %>%
  filter(USPS %in% c("DE","AR","MD","MO","NE","OK")) %>%
  ggplot() + 
  geom_col(aes(x=date, y=incidD_new, fill="New"), color="tomato", position="identity") +
  geom_col(aes(x=date, y=incidD, fill="Old"),color="steelblue",alpha=0.00001, position="identity") +
  scale_fill_manual(name="Type", values=c("New"="tomato", "Old"="steelblue")) +
  labs(x="Date", y="Reported Deaths") +
  facet_wrap(~state, scales="free_y", nrow=3, ncol=2)+
  guides(fill=guide_legend(override.aes=list(color=c("transparent","transparent"))))  +
  theme(legend.position = c(0.98,0.98),
        legend.key.size=unit(1, "cm"))

# final <-temp2 %>%
#   filter(USPS=="AL_H") %>%
#   select(date,incidD_new,USPS) %>%
#   mutate(State="Alabama_Hypothetical")%>%
#   bind_rows(temp2 %>% 
#               filter(USPS=="AL") %>% 
#               select(date,incidD_new,USPS) %>% 
#               mutate(State="Alabama")) %>%
#   rename(Deaths=incidD_new,Date=date) %>%
#   mutate(Date=ifelse(USPS=="AL_H",Date+35,Date)) %>%
#   filter(Date >=as.Date("2021-04-24",format="%Y-%m-%d") & 
#            Date<=as.Date("2021-11-20",format="%Y-%m-%d"))
# 
# 
# final %>%
#   #filter(USPS==abr) %>%
#   ggplot()+ geom_line(aes(x=Date,y=Deaths,color=State),position = "identity",size=2)+
#   labs(xlab="Date",ylab="Deaths")
#   #geom_col(aes(x=date,y=incidD),color="black",alpha=0.00001,position = "identity")
# 
# final<-temp2 %>%
#   filter(USPS=="AL_H") %>%
#   select(date,incidD_new) %>%
#   mutate(USPS="AL",date=date+35) %>%
#   left_join(temp2 %>% filter(USPS=="AL") %>% select(USPS,date,incidD)) %>%
#   filter(date >=as.Date("2021-04-24",format="%Y-%m-%d") & 
#            date<=as.Date("2021-11-20",format="%Y-%m-%d")) %>%
#   mutate(diff=mean(abs(incidD_new-incidD)))
#          
#          
#          diff=diff*diff,
#          diff=mean(diff),
#          diff=sqrt(diff))
