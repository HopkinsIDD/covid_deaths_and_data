#install.packages("dplyr")
install.packages("MMWRweek")
install.packages("lubridate")
library(lubridate)
library(MMWRweek)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

#PATH TO COVIDCOMMON PACKAGE IN CSP
install.packages("C:/Users/gupta/OneDrive/Documents/GitHub/COVIDScenarioPipeline/R/pkgs/covidcommon", type='source', repos=NULL, force = TRUE)

csse<-covidcommon::get_CSSE_US_data()

#PATH TO THE CDC DATA
cdc<-read.csv("C:/Users/gupta/OneDrive/Desktop/Notes/IDD Research/COVID-19/Thesis/Provisional_COVID-19_Death_Counts_by_Week_Ending_Date_and_State.csv")
cdc$COVID.19.Deaths[is.na(cdc$COVID.19.Deaths)]<-0
cdc<-cdc %>% mutate(Start.Date=as.Date(cdc$Start.Date,format="%m/%d/%Y"))
cdc<-cdc %>% mutate(End.Date=as.Date(cdc$End.Date,format="%m/%d/%Y"))
cdc<-cdc %>% filter(State!="United States")
cdc<-cdc %>% filter(Group=="By Week")
states<-unique(cdc$State)
#m<-max(cdc$COVID.19.Deaths[cdc$COVID.19.Deaths<max(cdc$COVID.19.Deaths)])

# cdc %>% ggplot(aes(x=Start.Date,y=COVID.19.Deaths))+
#   geom_col()+xlab("Time(in Weeks)") +
#   ylab("Deaths per Week") + 
#   facet_wrap(~State) +
#   ylim(0,1000) 

csse$incidDeath[is.null(csse$incidDeath)]<-0

# csse %>% ggplot(aes(x=Update,y=incidDeath))+
#   geom_col()+xlab("Time(in Weeks)") +
#   ylab("Deaths per Week") + 
#   facet_wrap(~source) +
#   ylim(0,1000) 

csse<-rename(csse,date=Update)
csse<-rename(csse,state=source)
csse<-rename(csse,incidD=incidDeath)

cdc_spec<-cdc[,c("Start.Date","End.Date","COVID.19.Deaths","State")]
gt_data<-csse[,c('date','source','FIPS','incidD')]
gt_data <- gt_data %>% filter(source != "US")

#Extracting the date per location per day
gt_data<-gt_data %>% group_by(source,date) %>%
  summarise(incidD=sum(incidD))

state_full=c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
"Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii",
"Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
"Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri",
"Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
"New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
"Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee",
"Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin",
"Wyoming")
state_short=c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
              "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
              "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
              "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")
final<-data.frame(matrix(ncol = 0, nrow = 0))
for (i in 1:length(state_full)){
#Running for Alaska - will run in a loop for different states
cdc_state<-cdc_spec %>% filter(State==state_full[i])
gt_state<-gt_data %>% filter(source==state_short[i])


###############################################################################
# If we need to do a comparison of death analysis we have to make sure that the
# start and end of both the data frame is on the same date. We do this because 
# we want the deaths calculated according to the same week allocations as the 
# CDC Data
###############################################################################

###Start Truncation
diff<-abs(cdc_state$Start.Date[1]-gt_state$date[1])
if(cdc_state$Start.Date[1]<gt_state$date[1]){
  df<-data.frame(source=rep("AK",diff),
                 date=seq.Date(from = cdc_state$Start.Date[1], 
                               to = gt_state$date[1]-1, by = 'days'),
                 incidD=rep(0,diff))
  gt_state<-rbind(df,gt_state)
}else{
  gt_state<-gt_state[24:nrows(gt_state),]
}

###END Truncation
diff<-abs(cdc_state$End.Date[nrow(cdc_state)]-gt_state$date[nrow(gt_state)])
if(cdc_state$End.Date[nrow(cdc_state)]<gt_state$date[nrow(gt_state)]){
  gt_state<-gt_state[1:(nrow(gt_state)-diff),]
}

gt_state$week_cdc<-rep(1:(nrow(gt_state)/7),each=7)
temp<-gt_state %>% group_by(week_cdc) %>%
  summarise(weeklyD=sum(incidD))

cdc_state$week_gt<-rep(1:nrow(cdc_state),each=1)
final_state<-cbind(cdc_state,temp)
if(!all(final_state$week_cdc==final_state$week_gt)){
  stop("Error the weeks do not match for the CDC and CSSE Data")
}

#final_state Data for State 
final_state<- final_state %>% select(-week_gt) %>%
  rename(week=week_cdc, CDC.Death=COVID.19.Deaths, CSSE.Death=weeklyD)

#final_state$state<-rep(state_full[i],each=nrow(final_state))
final<-rbind(final,final_state)


}
final %>% ggplot()+
  geom_col(aes(x=Start.Date,y=CSSE.Death,fill="CSSE"))+ 
  geom_col(aes(x=Start.Date,y=CDC.Death,fill="CDC"))+
  labs(fill="Orign")+
  xlab("Time(in Weeks)") +
  ylab("Deaths per Week")+
  facet_wrap(~State)+
  ylim(0,1000) 


#FOR INDIVIDUAL PLOTS 
# cdc_spec %>% ggplot() +
#   geom_col(aes(x=Start.Date,y=COVID.19.Deaths))+
#   facet_wrap(~State)+
#   ylim(0,1000) 
# 
# gt_data %>% ggplot() +
#   geom_col(aes(x=date,y=incidD))+
#   facet_wrap(~source)+
#   ylim(0,600) 

res <- csse %>% 
  mutate(date=lubridate::as_date(date), ## data is for previous day flu admission
         week=epiweek(date),
         year=epiyear(date),
         datew=MMWRweek2Date(year, week, 7)) %>%
  dplyr::select(-c(date, week, year, contains("cum"))) %>%
  group_by(state, datew) %>%  ##summarize daily data into weekly counts
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%  # can change this to be individual variables if you want
  ungroup() %>%
  mutate(date = datew)

res %>% ggplot() +
  geom_col(aes(x=date,y=incidDeath))+
  facet_wrap(~state)+
  ylim(0,1000)
