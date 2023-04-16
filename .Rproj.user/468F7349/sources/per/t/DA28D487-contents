library(tidyverse)
detach("package:DaCODeBED", unload = TRUE)
install.packages("C:/Users/gupta/OneDrive/Documents/GitHub/covid_deaths_and_data/DaCODeBED",type='source', repos=NULL, force = TRUE)
library(DaCODeBED)
datD<-read.csv("C:/Users/gupta/OneDrive/Documents/GitHub/covid_deaths_and_data/final/data/CSSE_WeeklyDeaths_DIRECT_DOWNLOAD.csv")
dat<- datD %>%
  select(incidD,date,USPS) %>%
  mutate(incidD =ifelse(incidD<0,0,incidD),
         date=as.Date(date,format="%m/%d/%Y"))

datS<-read.csv("C:/Users/gupta/OneDrive/Documents/GitHub/covid_deaths_and_data/final/data/CSSE_WeeklyDeaths_CSSE_Correction_Records.csv")
datS<- datS %>%
  select(date,USPS,out_csse=out,no,back_date) %>%
  mutate(date=as.Date(date,format="%m/%d/%Y"),
         back_date=as.Date(back_date,format="%m/%d/%Y"),
         out_csse=ifelse(is.na(out_csse),0,out_csse),
         no=ifelse(is.na(no),0,no))
datI<-read_csv("C:/Users/gupta/OneDrive/Documents/GitHub/covid_deaths_and_data/final/data/CSSE_WeeklyCasess_DIRECT_DOWNLOAD.csv")
datI<- datI %>%
  select(incidI,date,USPS) %>%
  mutate(incidI =ifelse(incidI<0,0,incidI))

dat<- dat %>%
  left_join(datI,by=c("USPS","date"))

dat<-dat %>%
  left_join(datS,by=c("date","USPS"))


dat<-dat %>%
  #filter(USPS=="MD") %>%
  group_by(USPS) %>%
  mutate(out_da=outlier_detection(date,incidD))

# dat%>%
#   ggplot() +
#   geom_col(aes(x=date,y=incidD,fill=factor(out_da))) +
#   facet_wrap(~USPS,scales="free_y")

temp2<- dat %>%
  group_by(USPS) %>%
  mutate(incidD_new=death_distribution(date,incidD,pois_out=out_da,use_cases =TRUE,incidI=incidI,
                                       support = FALSE))#,out_csse = out_csse,
                                      # diff = no,low_date = back_date))

final<- temp2 %>%
  select(date,incidD_new,USPS,out_da) %>%
  mutate(TYPE="new") %>%
  rename(incidD=incidD_new) %>%
  bind_rows(temp2 %>%
              select(date,incidD,USPS,out_da) %>%
              mutate(TYPE="old"))

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

# temp2_t <- temp2 %>% left_join(state_names)
# temp2_t$state<-factor(temp2_t$state,levels = unique(temp2_t$state),ordered=TRUE)
temp2 %>%
  filter(USPS %in% c("DE","AR","MD","MO","NE","OK")) %>%
  ggplot() + 
  geom_col(aes(x=date, y=incidD_new, fill="New"), color="tomato", position="identity") +
  geom_col(aes(x=date, y=incidD, fill="Old"),color="steelblue",alpha=0.00001, position="identity") +
  scale_fill_manual(name="Type", values=c("New"="tomato", "Old"="steelblue")) +
  labs(x="Date", y="Reported Deaths") +
  facet_wrap(~USPS, scales="free_y", nrow=3, ncol=2)+
  guides(fill=guide_legend(override.aes=list(color=c("transparent","transparent"))))  +
  theme(legend.position = c(0.98,0.98),
        legend.key.size=unit(1, "cm"))
