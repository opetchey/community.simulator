#' Simulate the population dynamics of a community of species using the Lotka-Volterra competition model with temperature-dependent vital rates.
#'
#' @param input_com_params Community object, containing all species and community parameters
#' @param TcelSeries Time series of temperature values
#' @param initial_abundances Initial abundances of each species
#'
#' @return Time series of population abundances for each species
#' @export
#'
#' @examples NULL
simulator_lv<-function(input_com_params,
                       TcelSeries,
                       initial_abundances){

  S<-input_com_params$S
  al<-input_com_params$alpha_ij
  bopt<-input_com_params$b_opt_i
  spread<-input_com_params$sd_perf_i
  if (is.null(spread)) {
    spread <- input_com_params$s_i
  }
  ab<-input_com_params$a_b_i
  ad<-input_com_params$a_d_i
  z<-input_com_params$z_i

  ##  density dependence constants
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
    b0<- ab * exp(-0.5 * ((Tcel - bopt) / spread)^2)
    d0<- ad * exp(z*Tcel)
    rms<-b0-d0 + 1e-6 # Ensuring a lower boundary for birth rate to prevent NAs...
    ## we investigated the effect of this constant and found that values of abundance
    ## were strongly correlated between simulations with and without this constant, and
    ## the cv of abundance across time was not affected by this constant

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

    ## immigration of 0.1 per time step
    ## WARNING: immigration rate is hard coded
    TimeSeries[,t+1] <- TimeSeries[,t+1] + 0.1

    ## this was an previously used method for avoiding
    ## very low abundances
    #if(any(TimeSeries[,t+1] <= 1e-4)==T) {
    #  TimeSeries[which(TimeSeries[,t+1] <= 1e-4),t+1] <- 1
    #}

    t <- t + 1
  }

  sp_ts<-TimeSeries[,-1] # drop 1st column as it's the initial abundance value you start with
  sp_ts<-t(sp_ts)
  Species_ID<-paste("Spp",1:ncol(sp_ts),sep="")
  colnames(sp_ts)<-Species_ID
  sp_ts<-as.data.frame(sp_ts)

  return(sp_ts)
}




