library(tidyverse)
library(ggplot2)
library(dplyr)

run_seir_model_discrete <- function(beta, sigma,gamma,omega, initial_cond, max_time, time_step) {
  times = seq(0, max_time, time_step)
  nsteps = length(times)

  S <- rep(NA, nsteps)
  S[1] = initial_cond["S"]
  
  E <- rep(NA, nsteps)
  E[1] = initial_cond["E"]
  
  I <- rep(NA, nsteps)
  I[1] = initial_cond["I"]
  
  R <- rep(NA, nsteps)
  R[1] = initial_cond["R"]
  
  incidI <- rep(NA, nsteps)
  incidI[1] = 0
  
  print(S[1])
  print(E[1])
  print(I[1])
  print(R[1])
  print(incidI[1])
  
  for (ii in 2:nsteps) {
    new_E = time_step * beta * max(S[ii - 1], 0)
    new_I = time_step * sigma * max(E[ii - 1], 0)
    new_R = time_step * gamma * max(I[ii - 1], 0)
    new_W = time_step * omega * max(R[ii - 1], 0)
    incidI[ii] = new_I
    
    S[ii] = S[(ii - 1)] - new_E + new_W
    E[ii] = E[(ii - 1)] + new_E - new_I 
    I[ii] = I[(ii - 1)] + new_I - new_R 
    R[ii] = R[(ii - 1)] + new_R - new_W
  }
  # Converts the ouput in a data frame
  out <- data.frame(cbind(times, S,E, I, R, incidI))
  return(out)
}

high_beta_seir<-run_seir_model_discrete(beta= 0.7, sigma=1/3, gamma = 1/10, omega=1/20,
                                        initial_cond = c(S = 5000000, E=0,I = 1, R =0),
                                        max_time = 50,time_step = 1)

# high_beta_seir %>%
#   ggplot()+
#   geom_line(aes(x=times,y=S),color="blue")+
#   geom_line(aes(x=times,y=E),color="red")+
#   geom_line(aes(x=times,y=I),color="green")+
#   geom_line(aes(x=times,y=R),color="black")+
#   geom_line(aes(x=times,y=S+E+I+R),color="orange")+
#   scale_color_manual(name="Legend 1",
#                      values=c("blue", "red","green","black","orange"))
#   #guides(fill=guide_legend(ncol=2))
#   #legend("topright", legend = c("S","E","I", "R","N"), col = c("blue", "red","green","black","orange"), pch = 16)

high_beta_seir<-high_beta_seir %>%
  mutate(N=S+E+I+R)

N=5000000+1
plot(NA, NA, xlim = c(min(high_beta_seir$times), 50), ylim=c(0,N),xlab = 'Time (In Days)', ylab = 'Number of Individuals',main='SEIR Model for hyptehtical pathogen')
#plotting lines for high beta
lines(high_beta_seir$times, high_beta_seir$S, col = 'blue', lty = 1,size=3)
lines(high_beta_seir$times, high_beta_seir$E, col = 'red', lty = 1,size=3)
lines(high_beta_seir$times, high_beta_seir$I, col = 'green', lty = 1,size=3)
lines(high_beta_seir$times, high_beta_seir$R, col = 'black', lty = 1,size=3)
lines(high_beta_seir$times, high_beta_seir$N, col = 'orange', lty = 1,size=3)
legend("topright", legend = c("S","E","I", "R","N"), col = c("blue", "red","green","black","orange"), pch = 16)
