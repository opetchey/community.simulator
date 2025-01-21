#rm(list=ls())

library(dirmult)

# function to get species response vs temperature timeseries

# Two Arguments:
# i) input_com_params = A list for 9 entries
#         1. S = species richness
#         2. a_b_i = intercepts for birth function (temp-dependent), a vector with values for each species
#         3. b_opt_i = species optimal temperature, a vector with values for each species
#         4. s_i = breadth of birth function, a vector with values for each species
#         5. a_d_i = intercepts for death function (temp-dependent), a vector with values for each species
#         6. z_i = scales the effect of temperature (in C) to mimic the Arrhenius relationship for death rate, a vector with values for each species
#         7. alpha_ij = competition coefficient, a S by S matrix
#         8. bet = density dependence constants, I kept it very small
#         9. delt = density dependence constants, I kept it very small

# ii) TcelSeries = a single row matrix with cell values equal to temperature 

# Output:
#   a data frame with species abundance along columns

simulator_lv<-function(input_com_params,
                       TcelSeries,
                       initial_abundances){
  
  S<-input_com_params$S
  al<-input_com_params$alpha_ij
  bopt<-input_com_params$b_opt_i
  spread<-input_com_params$s_i
  ab<-input_com_params$a_b_i
  ad<-input_com_params$a_d_i
  z<-input_com_params$z_i
  
  bet<-delt<- 0.001
  
  tot_time<-ncol(TcelSeries)
  #TcelSeries<-temperature_treatments$temperature_pulse
  #TcelSeries<-matrix(TcelSeries, nrow=1, ncol=tot_time)
    
  
  # initialization
  TimeSeries <- matrix(0, nrow=S,ncol= tot_time+1)# species along row
  # initial number of individuals
  NiTh <- initial_abundances # initial value of abundance
  TimeSeries[,1] <- NiTh 
  
  t <- 1
  while(t <= tot_time) {
    
    Nt <- as.matrix(TimeSeries[,t])
    Tcel<- TcelSeries[,t]
    
    # temperature-dependent vital rates
    b0<- ab * exp(-(Tcel - bopt)^2/spread)
    d0<- ad * exp(z*Tcel)
    
    rms<-b0-d0
    K<-rms/(bet+delt)
    #print(K)
    
    # main dynamics: L-V competition with temp-dependence rates
    myrate <- rms*(1 - (al%*%Nt)/K)
    logNtnext<- log(Nt) + myrate
    Ntnext=exp(logNtnext)
    
    #Ntnext<- Nt + Nt * rms*(1 - (al%*%Nt)/K)
    
    # print(al)
    # print("rms")
    # print(rms)
    # print("K")
    # print(K)
    # print("al%*%Nt")
    # print(al%*%Nt)
    # print("(1 - (al%*%Nt)/K)")
    # print((1 - (al%*%Nt)/K))
    # 
    
    TimeSeries[,t+1]<- Ntnext
    
    if(any(TimeSeries[,t+1] <= 1e-4)==T) {
      TimeSeries[which(TimeSeries[,t+1] <= 1e-4),t+1] <- 1
    }
    t <- t + 1
  }
  
  sp_ts<-TimeSeries[,-1] # drop 1st column as it's the initial abundance value you start with
  sp_ts<-t(sp_ts)
  Species_ID<-paste("Spp",1:ncol(sp_ts),sep="")
  colnames(sp_ts)<-Species_ID
  sp_ts<-as.data.frame(sp_ts)
  
  return(sp_ts)
}






simulator_lv_old <-function(input_com_params,
                       TcelSeries,
                       initial_abundances){
  
  S<-input_com_params$S
  al<-input_com_params$alpha_ij
  bopt<-input_com_params$b_opt_i
  spread<-input_com_params$s_i
  ab<-input_com_params$a_b_i
  ad<-input_com_params$a_d_i
  z<-input_com_params$z_i
  
  bet<-delt<-0.001
  
  tot_time<-ncol(TcelSeries)
  #TcelSeries<-temperature_treatments$temperature_pulse
  #TcelSeries<-matrix(TcelSeries, nrow=1, ncol=tot_time)
  
  
  # initialization
  TimeSeries <- matrix(0, nrow=S,ncol= tot_time+1)# species along row
  # initial number of individuals
  NiTh <- initial_abundances # initial value of abundance
  TimeSeries[,1] <- NiTh 
  
  t <- 1
  while(t <= tot_time) {
    
    Nt <- as.matrix(TimeSeries[,t])
    Tcel<- TcelSeries[,t]
    
    # temperature-dependent vital rates
    b0<- ab * exp(-(Tcel - bopt)^2/spread)
    d0<- ad * exp(z*Tcel)
    
    rms<-b0-d0
    K<-rms/(bet+delt)
    
    # main dynamics: L-V competition with temp-dependence rates
    Ntnext<- Nt + rms*(1 - (al%*%Nt)/K)
    
    TimeSeries[,t+1]<- Ntnext
    
    if(any(TimeSeries[,t+1] <= 1e-4)==T) {
      TimeSeries[which(TimeSeries[,t+1] <= 1e-4),t+1] <- 1
    }
    t <- t + 1
  }
  
  sp_ts<-TimeSeries[,-1] # drop 1st column as it's the initial abundance value you start with
  sp_ts<-t(sp_ts)
  Species_ID<-paste("Spp",1:ncol(sp_ts),sep="")
  colnames(sp_ts)<-Species_ID
  sp_ts<-as.data.frame(sp_ts)
  
  return(sp_ts)
}

