## prelims ----

#rm(list = ls())

library(tidyverse)
library(readxl)
library(here)
library(kableExtra)
library(patchwork)
library(primer)

source(here("R/0-functions/make a community.R"))



rep_names <- paste0("rep-", 1:num_replicates)

## make experiment ----
expt <- expand.grid(b_opt_mean = b_opt_mean_treatment,
                    b_opt_range = b_opt_range_treatment,
                    alpha_ij_mean = alpha_ij_mean_treatment,
                    alpha_ij_sd = alpha_ij_sd_treatment,
                    richness = richness_treatment,
                    rep_names = rep_names,
                    trait_selection_method = trait_selection_methods,
                    temperature_series_control = temperature_series_controls)
expt <- expt %>%
  mutate(community_id = paste0("Comm-", 1:nrow(expt)),
         case_id = paste(community_id, rep_names, sep = "-"))

## make communities ----
#S <- 10
a_b <- 0.3 #3
s <- 10
a_d <- 0 # 0.01
z <- 0.05
#alpha_ij_mean <- 0 # 0.5
#alpha_ij_sd <- 0 #0.1
intrafactor <- 1
community_object <- expt %>%
  rowwise(community_id) %>%
  #group_by(b_opt_mean, b_opt_range, rep_names, community_id) %>%
  do(community_object = make_a_community(S = .$richness,
                                         a_b = a_b,
                                         b_opt_mean = .$b_opt_mean,
                                         b_opt_range = .$b_opt_range,
                                         s = s,
                                         a_d = a_d,
                                         z = z,
                                         alpha_ij_mean = .$alpha_ij_mean,
                                         alpha_ij_sd = .$alpha_ij_sd,
                                         intrafactor = intrafactor,
                                         trait_selection_method = .$trait_selection_method))
expt <- cbind(expt, community_object)
saveRDS(expt, here("data", pack, "expt_communities.RDS"))


# ## Create the environment ----
# temperature_mean <- 20
# temperature_sd <- 4
# temperature_rate <- 50
# burn_in <- 2000
# expt_trt <- 1000
# temperature_series <- tibble(phase = c(rep("burn_in", burn_in), rep("expt", expt_trt+1)),
#                              time = 0:(expt_trt + burn_in),
#                              temperature = c(rep(temperature_mean, burn_in),
#                                              scale(one_over_f(gamma = 0.8, N = expt_trt+1)) *
#                                                temperature_sd + temperature_mean))
# 
#mean(temperature_series$temperature)
#sd(temperature_series$temperature)

#ggplot(temperature_series, aes(x = time, y = temperature)) +
#  geom_point() + geom_line()

#saveRDS(temperature_series, here("data", pack, "temperature_treatments.RDS"))






