pkgname <- "community.simulator"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('community.simulator')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("create_environments")
### * create_environments

flush(stderr()); flush(stdout())

### Name: create_environments
### Title: Create temperature times series. Currently three options for how
###   times series vary among cases.  1. 'same_per_replicate' replicates of
###   the same number share the same environmental time series. E.g.,
###   'case1_rep1' and 'case2_rep1' share the same time series.  2.
###   'all_different' all environmental time series are different
### Aliases: create_environments

### ** Examples

NULL



cleanEx()
nameEx("create_experiment_folder")
### * create_experiment_folder

flush(stderr()); flush(stdout())

### Name: create_experiment_folder
### Title: Create folder for experiment
### Aliases: create_experiment_folder

### ** Examples

NULL



cleanEx()
nameEx("create_experiment_table")
### * create_experiment_table

flush(stderr()); flush(stdout())

### Name: create_experiment_table
### Title: This function takes the experimental design from the specified
###   JSON experiment design file and creates a table of all the
###   simulations that will be run.
### Aliases: create_experiment_table

### ** Examples

NULL



cleanEx()
nameEx("get_arbitrary_derivatives")
### * get_arbitrary_derivatives

flush(stderr()); flush(stdout())

### Name: get_arbitrary_derivatives
### Title: Get derivatives at arbitrary temperatures
### Aliases: get_arbitrary_derivatives

### ** Examples

NULL



cleanEx()
nameEx("get_community_CPC_measures")
### * get_community_CPC_measures

flush(stderr()); flush(stdout())

### Name: get_community_CPC_measures
### Title: Get measures of community performance curves
### Aliases: get_community_CPC_measures

### ** Examples

NULL



cleanEx()
nameEx("get_community_CV")
### * get_community_CV

flush(stderr()); flush(stdout())

### Name: get_community_CV
### Title: Get the stability of each case (community). Calculated CV of
###   total biomass temporal variation. Also returns mean total biomass and
###   standard deviation of total biomass
### Aliases: get_community_CV

### ** Examples

NULL



cleanEx()
nameEx("get_community_measures")
### * get_community_measures

flush(stderr()); flush(stdout())

### Name: get_community_measures
### Title: Get various community level measures, e.g., community stability,
###   response diversity, position of optimal temperature, etc.
### Aliases: get_community_measures

### ** Examples

NULL



cleanEx()
nameEx("get_community_popstab")
### * get_community_popstab

flush(stderr()); flush(stdout())

### Name: get_community_popstab
### Title: Get the stability of each case (community). Calculated CV of
###   total biomass temporal variation. Also returns mean total biomass and
###   standard deviation of total biomass
### Aliases: get_community_popstab

### ** Examples

NULL



cleanEx()
nameEx("get_community_response_diversity")
### * get_community_response_diversity

flush(stderr()); flush(stdout())

### Name: get_community_response_diversity
### Title: Get measures of community response diversity
### Aliases: get_community_response_diversity

### ** Examples

NULL



cleanEx()
nameEx("get_community_sum_derivatives")
### * get_community_sum_derivatives

flush(stderr()); flush(stdout())

### Name: get_community_sum_derivatives
### Title: Get the sum of the communities derivatives at arbitrary
###   temperatures and at actual (temp = temporal) temperatures
### Aliases: get_community_sum_derivatives

### ** Examples

NULL



cleanEx()
nameEx("get_community_sum_rel_b_opt")
### * get_community_sum_rel_b_opt

flush(stderr()); flush(stdout())

### Name: get_community_sum_rel_b_opt
### Title: Get the sum of the relative position of the temperature optimum
###   of each species. Relative to the mean of the environmental
###   temperatures experienced by the community.
### Aliases: get_community_sum_rel_b_opt

### ** Examples

NULL



cleanEx()
nameEx("get_community_syn")
### * get_community_syn

flush(stderr()); flush(stdout())

### Name: get_community_syn
### Title: Get the stability of each case (community). Calculated CV of
###   total biomass temporal variation. Also returns mean total biomass and
###   standard deviation of total biomass
### Aliases: get_community_syn

### ** Examples

NULL



cleanEx()
nameEx("get_community_temp_sens")
### * get_community_temp_sens

flush(stderr()); flush(stdout())

### Name: get_community_temp_sens
### Title: Get the sensitivity of total biomass to temperature variation.
###   Currently measured as the slope of a linear regression of total
###   biomass on temperature.
### Aliases: get_community_temp_sens

### ** Examples

NULL



cleanEx()
nameEx("get_delta_igr")
### * get_delta_igr

flush(stderr()); flush(stdout())

### Name: get_delta_igr
### Title: Calculate the difference in growth rate from one time point to
###   the next
### Aliases: get_delta_igr

### ** Examples

NULL



cleanEx()
nameEx("get_temporal_derivatives")
### * get_temporal_derivatives

flush(stderr()); flush(stdout())

### Name: get_temporal_derivatives
### Title: Calculate the derivatives of the growth rate - temperature
###   relationship at the temperatures in the temperature times series of
###   each case in the experiment.
### Aliases: get_temporal_derivatives

### ** Examples

NULL



cleanEx()
nameEx("intrinsic_growth_gaussian")
### * intrinsic_growth_gaussian

flush(stderr()); flush(stdout())

### Name: intrinsic_growth_gaussian
### Title: Calculate intrinsic growth rate from species parameters and a
###   temperature, assuming a Gaussian birth rate - temperature response
###   curve and an exponential death rate - temperature response curve.
### Aliases: intrinsic_growth_gaussian

### ** Examples

NULL



cleanEx()
nameEx("lm_with_inf_check")
### * lm_with_inf_check

flush(stderr()); flush(stdout())

### Name: lm_with_inf_check
### Title: Linear model with infinite value check
### Aliases: lm_with_inf_check

### ** Examples

NULL



cleanEx()
nameEx("make_a_community")
### * make_a_community

flush(stderr()); flush(stdout())

### Name: make_a_community
### Title: This function makes a community object, which contains all the
###   parameters of the all speices in the community. At the moment only
###   the temperature optima $b_opt_i$ can vary among the species. The
###   other parameters are the same for all species.
### Aliases: make_a_community

### ** Examples

NULL



cleanEx()
nameEx("make_plots_for_one_community")
### * make_plots_for_one_community

flush(stderr()); flush(stdout())

### Name: make_plots_for_one_community
### Title: Make graphs showing variance results from one case
### Aliases: make_plots_for_one_community

### ** Examples

NULL



cleanEx()
nameEx("read_experiment_design_json")
### * read_experiment_design_json

flush(stderr()); flush(stdout())

### Name: read_experiment_design_json
### Title: Read in the JSON formatted text file that contains the
###   experiment design. All values in the JSON file must be expressions
###   that can be evaluated (using the 'eval' function) to get the values
###   of the experiment design.
### Aliases: read_experiment_design_json

### ** Examples

NULL



cleanEx()
nameEx("simulate_dynamics")
### * simulate_dynamics

flush(stderr()); flush(stdout())

### Name: simulate_dynamics
### Title: Simulate the dynamics of all the cases in an experiment
###   Unfortunately at the moment has features of temperature series hard
###   coded in
### Aliases: simulate_dynamics

### ** Examples

NULL



cleanEx()
nameEx("simulator_lv")
### * simulator_lv

flush(stderr()); flush(stdout())

### Name: simulator_lv
### Title: Simulate the population dynamics of a community of species using
###   the Lotka-Volterra competition model with temperature-dependent vital
###   rates.
### Aliases: simulator_lv

### ** Examples

NULL



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
