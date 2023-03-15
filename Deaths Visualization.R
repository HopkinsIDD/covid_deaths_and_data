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

csse <- covidcommon::get_CSSE_US_data(incl_unassigned = TRUE)


# PULL DATA ---------------------------------------------------------------

download.file(url = "https://data.cdc.gov/api/views/r8kw-7aab/rows.csv?accessType=DOWNLOAD", 
              destfile = "data/cdc_deaths.csv")




#PATH TO THE CDC DATA
cdc <- read_csv("data/cdc_deaths.csv")


cdc <- cdc %>% 
    mutate(start_date = lubridate::as_date(as.Date(`Start Date`, format="%m/%d/%Y")),
           end_date = lubridate::as_date(as.Date(`End Date`, format="%m/%d/%Y")))

cdc<-cdc %>% filter(State!="United States")
cdc<-cdc %>% filter(Group=="By Week")



cdc <- cdc %>% select(start_date, end_date, incidD=`COVID-19 Deaths`, state=State)

gt_data <- csse %>% select(date=Update, state=source, FIPS, incidD=incidDeath)
gt_data <- gt_data %>% filter(state != "US")

gt_dataI <- csse %>% select(date=Update, state=source, FIPS, incidI)
gt_dataI<- gt_dataI %>% filter(state != "US")

#Extracting the date per location per day
gt_data <- gt_data %>%
    group_by(state, date) %>%
    summarise(incidD=sum(incidD, na.rm = TRUE))

gt_dataI <- gt_dataI %>% 
  group_by(state, date) %>%
  summarise(incidI=sum(incidI, na.rm = TRUE))

# state_full=c("Alabama","Alaska","Arizona","Arkansas","California","Colorado",
#              "Connecticut","Delaware","District of Columbia","Florida","Georgia","Hawaii",
#              "Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine",
#              "Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri",
#              "Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico",
#              "New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon",
#              "Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee",
#              "Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin",
#              "Wyoming")
# 
# state_short=c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
#               "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
#               "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
#               "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")


state_names <- tibble(state = c(state.name, "Puerto Rico", "District of Columbia"), 
                      USPS = c(state.abb, "PR", "DC"))

cdc <- cdc %>% left_join(state_names) 
cdc <- cdc %>% filter(!is.na(USPS))

gt_data <- gt_data %>% rename(USPS = state) %>% left_join(state_names)
gt_data <- gt_data %>% filter(!is.na(USPS))

gt_dataI <- gt_dataI %>% rename(USPS = state) %>% left_join(state_names) 
gt_dataI <- gt_dataI %>% filter(!is.na(USPS))

final<-data.frame(matrix(ncol = 0, nrow = 0))


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

gt_dataI <- gt_dataI %>% 
  mutate(date=lubridate::as_date(date), ## data is for previous day flu admission
         week=epiweek(date),
         year=epiyear(date),
         datew=MMWRweek2Date(year, week, 7)) %>%
  dplyr::select(-c(date, week, year, contains("cum"))) %>%
  group_by(state, USPS, datew) %>%  ##summarize daily data into weekly counts
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%  # can change this to be individual variables if you want
  as_tibble() %>%
  mutate(date = datew)


final <- cdc %>%
    mutate(source = "cdc") %>%
    rename(date = end_date) %>%
    bind_rows(
        gt_data %>% mutate(source = "csse")
    )


final %>% 
    #filter(USPS %in% c("AL"),source=='csse') %>%
    #filter(source=='csse') %>%
    ggplot() +
    geom_col(aes(x=date, y=incidD, color=source,fill=source), position = position_dodge())+
    labs(titles='Deaths for all states',x='Date',y='Deaths')+
    coord_cartesian(ylim = c(0, 6000))+
    facet_wrap(~state)#, ncol = 2)



write_csv(gt_data,file="data/CSSE_WeeklyDeaths_incl_assigned.csv")
write_csv(gt_dataI,file="data/CSSE_WeeklyCases_incl_assigned.csv")

data%>%
  filter(USPS %in% c("NY","FL","TN","MO"))%>%
  ggplot()+ geom_col(aes(x=date,y=incidD))+
  labs(title="Deaths for 4 States",x="Date",y="Deaths")+
  coord_cartesian(ylim = c(0, 6000))+
  facet_wrap(~state)

# Making groups for visualization
groups<- data %>% group_by(USPS) %>% summarise(max=max(incidD))
q1<-quantile(groups$max, 0.25)
q2<-quantile(groups$max, 0.50)
q3<-quantile(groups$max, 0.75)

q1_g<- groups %>% filter(max<=q1) %>% select(USPS)
q2_g<- groups %>% filter(max>q1 & max<=q2) %>% select(USPS)
q3_g<- groups %>% filter(max>q2 & max<=q3) %>% select(USPS)
q4_g<- groups %>% filter(max>q3)%>% select(USPS)

q1_lim<-150
q2_lim<-500
q3_lim<-1200
q4_lim<-5000
