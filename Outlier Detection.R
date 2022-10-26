library(tidyverse)
library(ggplot2)


source("Deaths Visualization.R")

dat<-read_csv("data/CDC_CSSE_WeeklyDeaths.csv")

dat<-dat%>%
  filter(source=='csse')%>%
  select(incidD,state,date,USPS)



# BOX PLOT ---------------------------------------------------------------
dat %>%
  filter(USPS %in% c("FL","NY","MS","TN")) %>%
  ggplot(aes(y=incidD))+
  geom_boxplot()+
  coord_cartesian(ylim = c(0, 7500))+
  labs(title="Box plot for 4 states",y='Deaths')+
  facet_wrap((~state),ncol =2)



# HAMPEL FILTER ---------------------------------------------------------------
dat<-dat %>% 
  group_by(USPS)%>% 
  mutate(hampel_out=factor(ifelse(incidD<median(incidD)-3*mad(incidD) | incidD>median(incidD)+3*mad(incidD),1,0)))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=hampel_out))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Hampel Filter",x="Date",y="Deaths")+
  facet_wrap(~state)


# CARLINGA METHOD ---------------------------------------------------------------
dat<-dat %>% 
  group_by(USPS)%>% 
  mutate(carlinga_out=factor(ifelse(incidD<median(incidD)-2.3*IQR(incidD) | incidD>median(incidD)+2.3*IQR(incidD),1,0)))

dat %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=carlinga_out))+
  coord_cartesian(ylim = c(0, 3000))+
  labs(title="Outlier Detection by Carlinga Method",x="Date",y="Deaths")+
  facet_wrap(~state)


