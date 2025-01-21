## To do
## 1. Same time series for each block of replicates
## 2. Create derivatives when the temperature series is fixed
## 3. Look at correlations between pairs of species' igr across temperature




rm(list = ls())
library(here)

## there is currently no random number seed set.

## simulations were run in "packs", with each pack creating
## a set of data files

## the code immediately below is for working on a pack-by-pack basis.
## if you would like to redo any calculations across all packs, see lower down...

## Pack-by-pack section ----

## simulations can take a few hours.
# pack1
## define the experimental treatments ----
# pack <- "pack1"
# b_opt_mean_treatment <- seq(15, 25, 2)
# b_opt_range_treatment <- seq(1, 9, 4) #c(1, 5, 9) # seq(3, 7, 1)
# alpha_ij_mean_treatment <- c(0)
# alpha_ij_sd_treatment <- c(0, 0.1)


# pack2
## define the experimental treatments ----
#pack <- "pack2"
#b_opt_mean_treatment <- seq(15, 25, 1)
#b_opt_range_treatment <- seq(1, 9, 1) #c(1, 5, 9) # seq(3, 7, 1)
#alpha_ij_mean_treatment <- c(0)
#alpha_ij_sd_treatment <- seq(0, 0.5, 0.1) #c(0, 0.1)
#richness_treatment <- c(2, 5, 10)
#num_replicates <- 5

# pack3
## define the experimental treatments ----
# pack <- "pack3"
# b_opt_mean_treatment <- c(15, 20, 25)
# b_opt_range_treatment <- c(5, 10, 15)
# alpha_ij_mean_treatment <- 1
# alpha_ij_sd_treatment <- seq(0, 0.5, 0.1) #c(0, 0.1)
# richness_treatment <- seq(2, 20, 2)
# num_replicates <- 5

# # pack4
# ## define the experimental treatments ----
pack_temp <- "pack6"
# ## Create the environment ----
temperature_mean <- 20
temperature_sd <- 4
#temperature_series_controls <- c("all_same"); num_replicates <- 1
temperature_series_controls <- c("all_different"); num_replicates <- 5
pack <- paste0(pack_temp, "_", temperature_series_controls)
# ## Durations
burn_in <- 1000
expt_trt <- 1000
# ## Community properties
b_opt_mean_treatment <- seq(-5, 45, length = 100) # c(15, 20, 25)
b_opt_range_treatment <- c(10) #c(2,10) # c(5, 10, 15)
alpha_ij_mean_treatment <- 0.0
alpha_ij_sd_treatment <- 0 # seq(0, 0.5, 0.1) #c(0, 0.1)
richness_treatment <- 10 # seq(2, 20, 2)
trait_selection_methods <- c("deterministic", "random1")



# Design experiment
# this also makes the communities
source(here("R/1-design/design_expt.R"))

nrow(expt)

# Run dynamical simulation of experiment
source(here("R/2-run/run experiment.r"))
## this makes the population dynamics (dynamics.db) and the temperature series (temperatures.db)

# Now we get growth rates and derivatives, just for convenient future use
source(here("R/3-analyse/get_derivs_temp_time_series.R"))
## makes the derivs.db
source(here("R/3-analyse/get_derivs_temp_arbitrary_sequence.R"))
## makes derivs_arbseq.db

# Now we make a dataset of community stability
#source(here("R/3-analyse/get_community_stability.R"))

# Now we make a dataset of community measures
source(here("R/3-analyse/get_community_measures.R"))


# use "1-simulations/reports/main-report.qmd" to make a report containing various results.

