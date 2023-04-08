#' @import tidyverse
#' @import npreg
#' @import stats
#' @import ggplot2
#' @import dplyr
#' @name outlier_detection
#' @title outlier_detection
#' Detect outlier using Combination 3 method
#'
#' @param date Vector containing dates across which the analysis has to be performed
#' @param incidD Vector containing reported deaths on each of the above mentioned dates
#'
#' @return Vector of length date indicating which data points in incidD are outliers
#' @export

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

#' @name death_distribution
#' @title death_distribution
#' Performs back distribution of data dumps
#'
#' @param date Vector containing dates across which the analysis has to be performed
#' @param incidD Vector containing reported deaths on each of the above mentioned dates
#' @param pois_out Vector containing indicators if incidD point at date is an outlier 1 or not 0
#' @param use_cases TRUE/FALSE to indicate if cases have to be used to back distribute default FALSE
#' @param incidI Vector containing reported cases on each of the above mentioned dates if use_cases is TRUE
#' @param support TRUE/FALSE to indicate if additional support data for outliers, extra cases and back distribution date is provided
#' @param out_sup Vector indicating if reported incidD at a date is an outlier by supporting data 1 or 0
#' @param diff Vector containing the number of extra deaths reported on a date in supporting data integer
#' @param low_date Vector containing information about the period across which the diff should be back distributed to date type
#'
#' @return Vector of length date indicating the wave number to which data points in incidD belong to
#' @export

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

  if(support){
  temp<- temp %>%
          mutate(pois_out= ifelse(pois_out ==1 | out_csse==1,1,0))
  }else{
    temp<-temp %>%
      mutate(diff=rep(0,length(incidD)),
             low_date=rep(NA,length(incidD)))
  }

  temp<-temp %>%
          select(-c(out_csse))

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
    dplyr::filter(pois_out==1)%>%
    pull(date)

  wavelength<-temp %>%
    group_by(wave) %>%
    summarise(wlength=length(wave))

  for(i in out){
    w<- temp %>%
      dplyr::filter(date==i) %>%
      pull(wave)

    wave_pos<- nrow(temp %>%
                      dplyr::filter(wave==w) %>%
                      dplyr::filter(date<=i))
    dist_length=wave_pos
    if(w!=1 & wave_pos< (wavelength %>% dplyr::filter(wave==w) %>% pull(wlength))/4){
      w=c(w-1,w)
      dist_length=dist_length +  wavelength %>% dplyr::filter(wave==w[1]) %>% pull(wlength)
    }
    zero_length<-calc_zero_length(temp$incidD,temp$date,d=i)
    date_l<-(temp %>% dplyr::filter(wave == w[1]) %>% pull(date))[1]
    if((zero_length+1)>dist_length){
      date_l=(temp %>% dplyr::filter(date<=i) %>% pull(date))
      date_l=date_l[length(date_l)- zero_length]
    }
    pos=which(temp %>% dplyr::filter(pois_out==1) %>% pull(date) == i)
    if(pos!=1){
      #if((temp %>% filter(pois_out==1) %>% pull(wave))[pos] ==
      if(w[1] == (temp %>% dplyr::filter(pois_out==1) %>% pull(wave))[pos-1]){
        date_l=((temp %>% dplyr::filter(pois_out==1) %>% pull(date))[pos-1])
        date_l=(temp %>% pull(date))[which(temp %>% pull(date) == date_l)+1]
      }}
    #if((temp %>% filter(date==i) %>% pull(low_date))!="2000-01-25"){
    if(!is.na((temp %>% dplyr::filter(date==i) %>% pull(low_date)))){
      date_l_s=temp %>% dplyr::filter(date==i) %>% pull(low_date)
      if(date_l_s<(temp %>% pull(date))[1]){
        date_l_s=(temp %>% pull(date))[1]
      }
      date_l_s=(temp %>% dplyr::filter(date<=date_l) %>% pull(date))[nrow(temp %>% dplyr::filter(date<=date_l))]
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
