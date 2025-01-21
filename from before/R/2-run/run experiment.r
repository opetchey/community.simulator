#rm(list = ls())

## Some preliminaries ----
#options(scipen = 1, digits = 3) #set to two decimal 
library(here)
library(tidyverse)
#library(gridExtra)
#source(here("R", "mtime.R"))
#seed <- 101

## source some functions ----
source(here("R/0-functions/simulator_lv.R"))
source(here("R/0-functions/make a community.R"))

#set.seed(seed)

## set up data base to save results into
library(DBI)
library(RSQLite)
file.remove(here("data", pack, "/dynamics.db"))
file.remove(here("data", pack, "/temperatures.db"))
conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
conn_temperatures <- dbConnect(RSQLite::SQLite(), here("data", pack, "/temperatures.db"))
#dbWriteTable(conn, "dynamics", dynamics)
#dbListTables(conn)
#dbDisconnect(conn)

## read in experimental design
## Created by code in "design_expt.r" script in the experiments/1-design folder
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
#temperature_treatments <- readRDS(here("data", pack, "temperature_treatments.RDS"))

## sets a seed, in case none is set below
seed.to.use <- 1234569 ## set.seed(as.numeric(Sys.time()))
# now set below

i= 4
for(i in 1:nrow(expt)){
  
  print(i)
  
  
  ## seed setting for temperature time series
  ## keep only the next line to have the same temperature time series for all replicates
  #set.seed(123458)
  ## keep the next line to have a different seed for each replicate
  if(expt$temperature_series_control[i] == "all_same")
    set.seed(seed.to.use)
  if(expt$temperature_series_control[i] == "all_different")
    set.seed(seed.to.use + abs(parse_number(as.character(expt$rep_names[i]))))
  
  temperature_series <- tibble(phase = c(rep("burn_in", burn_in), rep("expt", expt_trt+1)),
                               time = 0:(expt_trt + burn_in),
                               temperature = c(rep(temperature_mean, burn_in),
                                               scale(one_over_f(gamma = 0.8, N = expt_trt+1)) *
                                                 temperature_sd + temperature_mean),
                               case_id = expt$case_id[i])
  Tcel_control<-temperature_series$temperature
  Tcel_controlm<-matrix(Tcel_control,nrow=1)
  
  
  
 
  
  S <- expt[i,]$community_object[[1]]$S
  
  initial_abundances <- (rdirichlet(1, rep(1, S))*1000)[1,]

    
  
  spts <- simulator_lv(input_com_params = expt$community_object[[i]],
                       TcelSeries = Tcel_controlm,
                       initial_abundances = initial_abundances)
  
  
  
  spts <- spts |> 
    as_tibble() |> 
    mutate(case_id = expt$case_id[i],
           time = temperature_series$time) |> 
    pivot_longer(names_to = "Species_ID", values_to = "Abundance",
                 cols = starts_with("Spp")) |> 
    filter(time > burn_in)
  
  temperature_series_expt_only <- temperature_series |> 
    filter(time > burn_in)
  
  if(i == 1) {
    dbWriteTable(conn_dynamics, "dynamics", spts, overwrite = TRUE)
    dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, overwrite = TRUE)
  }
  if(i > 1) {
    dbWriteTable(conn_dynamics, "dynamics", spts, append = TRUE)
    dbWriteTable(conn_temperatures, "temperatures", temperature_series_expt_only, append = TRUE)
  }
}

#dynamics <- tbl(conn, "dynamics") |> 
#  collect()


