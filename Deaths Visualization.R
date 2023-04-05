#install.packages("dplyr")
install.packages("MMWRweek")
install.packages("lubridate")
library(lubridate)
library(MMWRweek)
library(dplyr)
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(dplyr)

state_names <- tibble(state = c(state.name, "Puerto Rico", "District of Columbia","Alabama Hypothtical"), 
                      USPS = c(state.abb, "PR", "DC","AL_H"))

## WITH COVID COMMON -----------------------------------------------------------

#PATH TO COVIDCOMMON PACKAGE IN CSP
install.packages("C:/Users/gupta/OneDrive/Documents/GitHub/COVIDScenarioPipeline/R/pkgs/covidcommon", type='source', repos=NULL, force = TRUE)

csse <- covidcommon::get_CSSE_US_data(incl_unassigned = TRUE)
gt_data <- csse %>% select(date=Update, state=source, FIPS, incidD=incidDeath)
gt_data <- gt_data %>% filter(state != "US")
gt_data <- gt_data %>%
  group_by(state, date) %>%
  summarise(incidD=sum(incidD, na.rm = TRUE))
gt_data <- gt_data %>% rename(USPS = state) %>% left_join(state_names)
gt_data <- gt_data %>% filter(!is.na(USPS))

gt_data <- gt_data %>%
  mutate(date=lubridate::as_date(date), ## data is for previous day flu admission
         week=epiweek(date),
         year=epiyear(date),
         datew=MMWRweek2Date(year, week, 7)) %>%
  dplyr::select(-c(date, week, year, contains("cum"))) %>%
  group_by(state, USPS, datew) %>%  ##summarize daily data into weekly counts
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%  # can change this to be individual variables if you want
  as_tibble() %>%
  mutate(date = datew)

pdf(file= "final/FINAL_Deaths_CSSE_COVID_COMMON.pdf" )
for (abr in  c(state.abb, "PR", "DC")){
  print(gt_data %>%
          filter(USPS==abr) %>%
          ggplot()+
          geom_col(aes(x=date,y=incidD))+
          labs(title=abr))
}
dev.off()

write_csv(gt_data,file="final/data/CSSE_WeeklyDeaths_COVID_COMMON.csv")

#DIRECTLY FROM CSSE ------------------------------------------------------------
csse_dat <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")

gt_data_n=data.frame(iso3=c(),Update=c(),source=c(),cumD=c(),incidD=c())
 for (abr in c(state.name, "Puerto Rico", "District of Columbia")){
#for (abr in c("Florida","Puerto Rico", "District of Columbia")){
  abr=tolower(abr)
temp_dat <- csse_dat %>% filter(tolower(Province_State) == abr) %>%
  select(iso3, source = Province_State, contains("/")) %>%
  pivot_longer(cols = -c(iso3, source), names_to = "Update", values_to = "cumD") %>%
  mutate(Update = lubridate::as_date(as.Date(Update, format = "%m/%d/%y"))) %>%
  group_by(iso3, Update, source) %>%
  summarise(cumD = sum(cumD, na.rm = TRUE)) %>%
  as_tibble() %>%
  arrange(iso3, source, Update) %>%
  mutate(incidD = diff(c(0,cumD)))
gt_data_n=rbind(gt_data_n,temp_dat)
 }

gt_data_n<-gt_data_n %>%
  select(date=Update,state=source,incidD)

gt_data_n<-gt_data_n %>% left_join(state_names)

gt_data_n<- gt_data_n %>%
  mutate(incidD=abs(incidD))

gt_data_n <-gt_data_n %>%
  mutate(date=lubridate::as_date(date), ## data is for previous day flu admission
         week=epiweek(date),
         year=epiyear(date),
         datew=MMWRweek2Date(year, week, 7)) %>%
  dplyr::select(-c(date, week, year, contains("cum"))) %>%
  group_by(state, USPS, datew) %>%  ##summarize daily data into weekly counts
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%  # can change this to be individual variables if you want
  as_tibble() %>%
  mutate(date = datew)



s<-sum(gt_data_n %>%
  filter(USPS=="AL",date >=as.Date("2021-04-24",format="%Y-%m-%d") & 
           date<=as.Date("2021-11-20",format="%Y-%m-%d")) %>%
  pull(incidD))

temp<-gt_data_n %>%
  filter(USPS=="AL") %>%
  mutate(state="Alabama_Hypothetical",
         USPS="AL_H",
         incidD=ifelse(date >=as.Date("2021-04-24",format="%Y-%m-%d") & 
                         date<as.Date("2021-11-20",format="%Y-%m-%d"),0,incidD ),
         incidD=ifelse(date==as.Date("2021-11-20",format="%Y-%m-%d"),s,incidD ))

gt_data_n <- rbind(gt_data_n,temp)

gt_data_n<- gt_data_n %>%
  mutate(date=as.Date(date,format="%Y-%m-%d"))

pdf(file= "final/FINAL_Deaths_CSSE_DIRECT_DOWNLOAD.pdf" )
for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(gt_data_n %>%
          filter(USPS==abr) %>%
          ggplot()+
          geom_col(aes(x=date,y=incidD))+
          labs(title=abr))
}
dev.off()

write_csv(gt_data_n,file="final/data/CSSE_WeeklyDeaths_DIRECT_DOWNLOAD.csv")


gt_data_n_t<-gt_data_n
gt_data_n_t$state<-factor(gt_data_n_t$state,levels = unique(gt_data_n_t$state),ordered=TRUE)

gt_data_n_t %>% 
  filter(USPS %in% c("AL_H")) %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD),color="tomato",fill="tomato")+
  labs(x="Date",y="Reported Deaths")+
  facet_wrap(~state,scales = "free_y",nrow = 3, ncol = 2)





#############################################################################
csse_cases<-read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

gt_dataI=data.frame(iso3=c(),Update=c(),source=c(),cumI=c(),incidI=c())
for (abr in c(state.name, "Puerto Rico", "District of Columbia")){
  #for (abr in c("Florida","Puerto Rico", "District of Columbia")){
  abr=tolower(abr)
  temp_dat <- csse_cases %>% filter(tolower(Province_State) == abr) %>%
    select(iso3, source = Province_State, contains(".")) %>%
    pivot_longer(cols = -c(iso3, source), names_to = "Update", values_to = "cumI") %>%
    mutate(Update = lubridate::as_date(as.Date(Update, format = "X%m.%d.%y"))) %>%
    group_by(iso3, Update, source) %>%
    summarise(cumI = sum(cumI, na.rm = TRUE)) %>%
    as_tibble() %>%
    arrange(iso3, source, Update) %>%
    mutate(incidI = diff(c(0,cumI)))
  gt_dataI=rbind(gt_dataI,temp_dat)
}

gt_dataI<-gt_dataI %>%
  select(date=Update,state=source,incidI)

gt_dataI<-gt_dataI %>% left_join(state_names)

gt_dataI<- gt_dataI %>%
  mutate(incidI=abs(incidI))

gt_dataI <-gt_dataI %>%
  mutate(date=lubridate::as_date(date), ## data is for previous day flu admission
         week=epiweek(date),
         year=epiyear(date),
         datew=MMWRweek2Date(year, week, 7)) %>%
  dplyr::select(-c(date, week, year, contains("cum"))) %>%
  group_by(state, USPS, datew) %>%  ##summarize daily data into weekly counts
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%  # can change this to be individual variables if you want
  as_tibble() %>%
  mutate(date = datew)

temp<-gt_dataI %>%
  filter(USPS=="AL") %>%
  mutate(state="Alabama_Hypothetical",
         USPS="AL_H")

gt_dataI<- rbind(gt_dataI,temp)

pdf(file= "final/FINAL_CASES_CSSE_DIRECT_DOWNLOAD.pdf" )
for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(gt_dataI %>%
          filter(USPS==abr) %>%
          ggplot()+
          geom_col(aes(x=date,y=incidI))+
          labs(title=abr))
}
dev.off()

write_csv(gt_dataI,file="final/data/CSSE_WeeklyCasess_DIRECT_DOWNLOAD.csv")

################################################################################

#CDC DATA

if(FALSE){ #SINCE NOT NEEDED EVERYTIME
download.file(url = "https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD", 
              destfile = "data/cdc_deaths.csv")

cdc <- read_csv("data/cdc_deaths.csv")

cdc <- cdc %>% 
  mutate(start_date = lubridate::as_date(as.Date(`Start Date`, format="%m/%d/%Y")),
         end_date = lubridate::as_date(as.Date(`End Date`, format="%m/%d/%Y")))

cdc<-cdc %>% filter(State!="United States")
cdc<-cdc %>% filter(Group=="By Week")

cdc <- cdc %>% select(start_date, end_date, incidD=`COVID-19 Deaths`, state=State)

cdc <- cdc %>% left_join(state_names) 
cdc <- cdc %>% filter(!is.na(USPS))
cdc<- cdc %>%
  mutate(incidD=ifelse(is.na(incidD),0,incidD)) %>%
  select(c(date=start_date,incidD,USPS,state))

final<-data.frame(matrix(ncol = 0, nrow = 0))

gt_data_n<- gt_data_n %>%
  select(-c(datew))


final <- cdc %>%
    mutate(source = "CDC") %>%
    #rename(date = end_date) %>%
    bind_rows(
        gt_data_n %>% mutate(source = "CSSE")
    )

final<- final %>%
  rename(Source=source)

pdf(file= "final/Deaths_CDC_CSSE_LAG.pdf" )
for (abr in  c(state.abb, "PR", "DC")){
print(final %>% 
    filter(USPS ==abr) %>%
    #filter(source=='csse') %>%
    ggplot() +
    geom_col(aes(x=date, y=incidD, color=Source,fill=Source), position = "identity")+
    labs(titles=abr,x='Date',y='Deaths'))#+
    #coord_cartesian(ylim = c(0, 6000))+
    ##facet_wrap(~state)#, ncol = 2)
}
dev.off()

final_t<-final
final_t$state<-factor(final$state,levels = unique(final$state),ordered=TRUE)

final_t %>% 
  filter(USPS %in% c("AR","DE","MD","MO","NE","OK")) %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(Source)),width=3.5)+
  theme(legend.position = c(0.97,1))+
  scale_fill_discrete(name="Data Source", labels=c("CDC", "CSSE"))+ 
  labs(x="Date",y="Reported Deaths")+
  facet_wrap(~state,scales = "free_y",nrow = 3, ncol = 2)



}


### GOLD STANDARD DATA ---------------------------------------------------------

#VISUAL GOLD STANDARD
gold_vis<- read.csv("final/data/CSSE_WeeklyDeaths_GoldStandard_Visual.csv")

gold_vis<- gold_vis %>%
  mutate(date=lubridate::as_date(as.Date(`date`, format="%m/%d/%Y")))

pdf(file= "final/FINAL_Deaths_CSSE_DirectDownload_VisualOutliers.pdf" )
for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(gold_vis %>%
          filter(USPS==abr) %>%
          ggplot()+
          geom_col(aes(x=date,y=incidD,fill=factor(gold_vis_out)))+
          labs(title=abr)+
          theme(legend.position = "none"))
}
dev.off()

# CSSE Correction Records
csse_correction <- read.csv("final/data/CSSE_WeeklyDeaths_CSSE_Correction_Records.csv")

csse_correction<- csse_correction %>%
  mutate(back_date=lubridate::as_date(as.Date(`back_date`, format="%m/%d/%Y")),
         date=lubridate::as_date(as.Date(`date`, format="%m/%d/%Y")))

csse_correction<- csse_correction %>%
  mutate(no=ifelse(is.na(no),0,no))

##COMBINED GOLDSTANDARD OUTLIERS
final_gold <- gold_vis %>%
  select(c(date,USPS,incidD,gold_vis_out)) %>%
  left_join(csse_correction %>% select(c(date,USPS,incidD,out))) %>% 
  mutate(gold_out=ifelse((out ==1 | gold_vis_out ==1),1,0)) %>%
  select(c(date,USPS,incidD,gold_out))

pdf(file= "final/FINAL_Deaths_CSSE_DirectDownload_FinalGoldOutliers.pdf" )
for (abr in  c(state.abb, "PR", "DC","AL_H")){
  print(final_gold %>%
          filter(USPS==abr) %>%
          ggplot()+
          geom_col(aes(x=date,y=incidD,fill=factor(gold_out)))+
          labs(title=abr)+
          theme(legend.position = "none"))
}
dev.off()

final_gold_t<-final_gold %>% left_join(state_names)
final_gold_t$state<-factor(final_gold_t$state,levels = unique(final_gold_t$state),ordered=TRUE)

final_gold_t %>% 
  filter(USPS %in% c("DE","AR","MD","MO","NE","OK")) %>%
  ggplot()+
  geom_col(aes(x=date,y=incidD,fill=factor(gold_out)))+
  theme(legend.position = c(0.96,1))+
  scale_fill_discrete(name="Type of Data Point", labels=c("Non-Outlier", "Outlier"))+ 
  labs(x="Date",y="Reported Deaths")+
  facet_wrap(~state,scales = "free_y",nrow = 3, ncol = 2)

write_csv(final_gold ,file="final/data/CSSE_WeeklyDeaths_Gold_Outliers.csv")
