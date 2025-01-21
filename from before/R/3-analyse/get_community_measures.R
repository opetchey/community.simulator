#rm(list=ls())

library(tidyverse)
library(readxl)
library(MESS)
library(here)
library(patchwork)
library(DBI)
library(RSQLite)
library(broom)

#pack <- "pack4_all_same"


## import datasets and open connections to data bases ----
## experimental design
expt <- readRDS(here("data", pack, "expt_communities.RDS"))
## community dynamics
conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
dynamics <- tbl(conn_dynamics, "dynamics")
## derivatives
conn_derivs <- dbConnect(RSQLite::SQLite(), here("data", pack, "/derivs.db"))
derivs <- tbl(conn_derivs, "derivs")
## arbitrary derivatives
conn_derivs_arb <- dbConnect(RSQLite::SQLite(), here("data", pack, "/derivs_arbseq.db"))
derivs_arb <- tbl(conn_derivs_arb, "derivs")
## temperature time series
conn_temperatures <- dbConnect(RSQLite::SQLite(), here("data", pack, "temperatures.db"))
temperatures <- tbl(conn_temperatures, "temperatures")


## Community biomass and community stability ----
temp1 <- dynamics |>
  group_by(case_id, time) %>%
  summarise(tot_ab = sum(Abundance, na.rm = T)) |>
  collect()
comm_stab <- temp1 %>%
  group_by(case_id) %>%
  summarise(mean_totab = mean(tot_ab),
            sd_totab = sd(tot_ab),
            CV_totab = sd_totab / mean_totab) |> 
  full_join(expt) |> 
  select(case_id, mean_totab, sd_totab, CV_totab, rep_names, community_id)


## calculate temperature sensitivity of total biomass
## get rolling sum of temperatures
temperatures <- temperatures |>
  collect()
temperatures <- temperatures |>
  group_by(case_id) |> 
  mutate(temperature_rollsum = zoo::rollsumr(temperature, 50, fill = NA))
## calculate the temperature sensitivity of total biomass
## merge the temperature and biomass time series
dd <- full_join(temp1, temperatures, by = c("case_id" = "case_id", "time" = "time")) |> 
  select(case_id, time, temperature, temperature_rollsum, tot_ab)
temp_sens <- dd |> 
  nest(data = c(time, temperature, temperature_rollsum, tot_ab)) |>
  mutate(model = map(data, ~ lm(tot_ab ~ temperature, data = .))) |> 
  mutate(tidy_model = map(model, tidy)) %>%
  unnest(tidy_model) |> 
  filter(term == "temperature")
temp_sens_to_merge_rs <- temp_sens %>%
  select(case_id, temperature_sensitivity_rs = estimate)


## response diversity ----
temp2 <- derivs |>
  group_by(case_id, temperature) %>%
  summarise(mean_deriv = mean(derivative),
            sum_deriv = sum(derivative),
            max_igr = max(igr)) |>
  collect()
summary_derivs <- temp2 |> 
  group_by(case_id) |> 
  summarise(mean_abs_deriv = mean(abs(mean_deriv)),
            sum_abs_deriv =  sum(abs(mean_deriv)),
            sum_max_igr = sum(max_igr))


## position of temperature optima relative to
## mean environmental temperature each simulation
mean_temps <- temperatures |>
  group_by(case_id) %>%
  summarise(mean_temperature = mean(temperature)) |>
  collect()
expt_long <- unnest_longer(expt, col = c(community_object)) |> 
  filter(community_object_id == "b_opt_i") |> 
  unnest(cols = c(community_object)) |> 
  ## DANGER next line hard coded number of species
  mutate(species_id = rep(paste0("Spp-", 1:10), length.out = length(community_object))) 
rel_b_opt <- full_join(mean_temps, expt_long) |> 
  mutate(relative_b_opt = community_object - mean_temperature)
comm_sum_rel_b_opt <- rel_b_opt |> 
  group_by(case_id) |> 
  summarise(sum_rel_b_opt = sum(relative_b_opt))


## get derivative of each species at mean temperature
spp_derivs_temp1 <- derivs_arb |>
  collect()
spp_derivs_temp2 <- spp_derivs_temp1 |>
  full_join(mean_temps) |> 
  mutate(temperature_diff = abs(temperature - mean_temperature)) |>
  group_by(case_id, species_id) |>
  filter(temperature_diff == min(temperature_diff)) |> 
  full_join(rel_b_opt)

## get mean derivative of each species
mean_derivs <- spp_derivs_temp2 |> 
  group_by(case_id) |> 
  summarise(sum_deriv = sum(derivative))

##ggplot(spp_derivs_temp2, aes(x = relative_b_opt, y = derivative)) +
##  geom_point()

comm_measures <- full_join(summary_derivs, comm_sum_rel_b_opt) |> 
  full_join(comm_stab) |> 
  full_join(temp_sens_to_merge_rs) |> 
  full_join(mean_derivs)

saveRDS(comm_measures, here("data", pack, "community_measures.RDS"))


