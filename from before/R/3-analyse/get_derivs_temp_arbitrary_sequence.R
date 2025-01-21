#rm(list = ls())

#library(dplyr)

source(here("R/0-functions/intrinsic_growth_rate.R"))

## read in experimental design
## Created by code in "design_expt.r" script in the experiments/1-design folder
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
#temperature_treatments <- readRDS(here("data", pack, "temperature_treatments.RDS")) %>%
#  filter(phase == "expt") %>%
#  select(-phase)


## set up data base to save results into
library(DBI)
library(RSQLite)
file.remove(here("data", pack, "/derivs_arbseq.db"))
conn_derivs <- dbConnect(RSQLite::SQLite(), here("data", pack, "/derivs_arbseq.db"))

## open connect to temperature time series
conn_temperatures <- dbConnect(RSQLite::SQLite(), here("data", pack, "temperatures.db"))
temperatures <- tbl(conn_temperatures, "temperatures")



## Expand expt to make a species in row dataset
i = 1

print(i)
case_id_oi <- expt$case_id[i]
temp1 <- temperatures |> 
  filter(case_id == case_id_oi) |> 
  collect() |> 
  summarise(min = min(temperature),
            max = max(temperature))
buffer <- 5
temperatures_oi <- tibble(temperature = seq(temp1$min-buffer,
                                            temp1$max+buffer,
                                            length = 200))




for(i in 1:length(expt$case_id)) {
  
  print(i)
  # case_id_oi <- expt$case_id[i]
  # temperatures_oi <- temperatures |> 
  #   filter(case_id == case_id_oi) |> 
  #   collect() |> 
  #   filter((time %% 10) == 0)
  #   
  # 
  
  comm_pars_i <- expt$community_object[i][[1]]
  
 # if(i == 1)
    species_pars <- tibble(case_id = rep(expt$case_id[i], length(comm_pars_i$b_opt_i)),
                           species_id = paste0("Spp-", 1:length(comm_pars_i$b_opt_i)),
                           b_opt_i = comm_pars_i$b_opt_i,
                           a_b_i = comm_pars_i$a_b_i,
                           s_i = comm_pars_i$s_i,
                           a_d_i = comm_pars_i$a_d_i,
                           z_i = comm_pars_i$z_i)
    
    species_pars1 <- species_pars %>%
      mutate(temperatures = list(temperatures_oi$temperature)) %>%
      unnest(cols = temperatures)
    
    species_pars2 <- species_pars1 %>%
      mutate(igr = intrinsic_growth_rate2(a_b_i,
                                          b_opt_i,
                                          s_i,
                                          a_d_i,
                                          z_i,
                                          temperatures))
    
    species_pars3 <- species_pars2 %>%
      nest_by(case_id, species_id) %>%
      mutate(models = list(mgcv::gam(igr ~ s(temperatures, k = 10),
                                     data = data))) %>%
      select(-data) 
    
    species_pars4 <- full_join(species_pars1, species_pars3) %>%
      group_by(case_id, species_id) %>%
      mutate(new_data = list(data.frame(temperatures = temperatures))) %>%
      select(-temperatures) %>%
      unique() %>%
      rowwise() %>%
      mutate(derivative = list(gratia::derivatives(models,
                                                   data = new_data))) %>%
      unnest(derivative) %>%
      select(-models, -new_data) %>%
      rename(temperature = temperatures, derivative = .derivative) %>%
      mutate(igr = intrinsic_growth_rate2(a_b_i,
                                          b_opt_i,
                                          s_i,
                                          a_d_i,
                                          z_i,
                                          temperature)) %>%
      select(case_id, species_id, temperature, igr, derivative)

    
    
    if(i == 1) {
      dbWriteTable(conn_derivs, "derivs", species_pars4, overwrite = TRUE)
    }
    if(i > 1) {
      dbWriteTable(conn_derivs, "derivs", species_pars4, append = TRUE)
    }  
    
}

dbDisconnect(conn_derivs)


# p1 <- species_pars2 |> 
#   filter(case_id == "Comm-94-rep-1") |> 
#   ggplot(aes(x = temperatures, y = igr, col = species_id)) +
#   geom_line()
# p1
# 
# 
# species_pars4 %>%
#   filter(case_id == "Comm-94-rep-1") %>%
#   ggplot(aes(x = temperature, y = igr)) +
#   geom_line(aes(y = igr)) +
#   geom_line(aes(y = derivative)) +
#   facet_wrap(vars(species_id))
# 
# 
# 
# 
# saveRDS(species_pars4, here("data", pack, "species_igr_deriv.RDS"))
