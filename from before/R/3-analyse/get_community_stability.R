#rm(list=ls())

library(tidyverse)
library(readxl)
library(MESS)
library(here)
library(patchwork)
library(DBI)
library(RSQLite)
library(broom)

#pack <- "pack2"

expt <- readRDS(here("data", pack, "expt_communities.RDS"))

conn_dynamics <- dbConnect(RSQLite::SQLite(), here("data", pack, "/dynamics.db"))
#dbListTables(conn_dynamics)
dynamics <- tbl(conn_dynamics, "dynamics")

# conn_derivs <- dbConnect(RSQLite::SQLite(), here("data", pack, "/derivs.db"))
# #dbListTables(conn_derivs)
# derivs <- tbl(conn_derivs, "derivs")

## open connect to temperature time series
conn_temperatures <- dbConnect(RSQLite::SQLite(), here("data", pack, "temperatures.db"))
temperatures <- tbl(conn_temperatures, "temperatures")



## Community level stability ----
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
  
#comm_stab <- comm_stab|> 
#  select(case_id, mean_totab, sd_totab, CV_totab, rep_names, community_id)


## calculate temperature sensitivity of total biomass
## get rolling sum of temperatures
temperatures <- temperatures |>
  collect()
temperatures <- temperatures |>
  group_by(case_id) |> 
  mutate(temperature_rollsum = zoo::rollsumr(temperature, 50, fill = NA))

## plot for one case_id the temperature through time
temperatures |> 
  filter(case_id == "Comm-1-rep-1") |>
ggplot(aes(x = time, y = temperature_rollsum/50)) +
  geom_line() +
  geom_line(aes(y = temperature), color = "red") +
  facet_wrap(~case_id, scales = "free_y")

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
  select(case_id, estimate, temperature_sensitivity_rs = estimate)


comm_stab <- comm_stab |>
  full_join(temp_sens_to_merge_rs, by = "case_id") |> 
  full_join(comm_sum_rel_b_opt)

# p1 <- comm_stab |> 
#   ggplot(aes(x = temperature_sensitivity_rs, y = CV_totab)) +
#   geom_point() +
#   geom_smooth()
# 
# p2 <- comm_stab |> 
#   ggplot(aes(x = sum_rel_b_opt, y = temperature_sensitivity_rs, col = CV_totab)) +
#   geom_point(size = 3) +
#   geom_smooth()
# 
# p3 <- comm_stab |> 
#   ggplot(aes(x = sum_rel_b_opt, y = CV_totab)) +
#   geom_point() +
#   geom_smooth()
# 
# p1 + p2 + p3

saveRDS(comm_stab, here("data", pack, "community_stability.RDS"))




## Population level stability ----
temp1 <- dynamics %>%
  group_by(case_id, Species_ID) %>%
  summarise(mean_pop = mean(Abundance),
            sd_pop = sd(Abundance)) |> 
  mutate(CV_pop = sd_pop / mean_pop) |> 
  collect()
pop_stab <- temp1 |> 
  full_join(expt) |> 
  select(case_id, mean_pop, sd_pop, CV_pop, rep_names, community_id, Species_ID)

#comm_stab <- comm_stab|> 
#  select(case_id, mean_totab, sd_totab, CV_totab, rep_names, community_id)


saveRDS(pop_stab, here("data", pack, "population_stability.RDS"))


