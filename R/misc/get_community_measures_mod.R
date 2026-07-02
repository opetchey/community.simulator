#' Get various community level measures, e.g., community stability, response diversity, position of optimal temperature, etc.
#'
#' @param experiment_folder The folder containing the experiment data
#' @param experiment_design_filename The name of the experiment design file
#'
#' @return Nothing. Saves data to a file.
#' @export
#'
#' @examples NULL
get_community_measures_mod <- function(experiment_folder,
                                   experiment_design_filename) {


  ## Read in experiment information
  expt <- readRDS(paste0(experiment_folder, "experiment_table.RDS"))
  expt_def <- jsonlite::fromJSON(paste0(experiment_folder, experiment_design_filename))

  ## open connections to databases
  ## temperatures
  conn_temperatures <- dbConnect(RSQLite::SQLite(),
                                 paste0(experiment_folder, "temperatures.db"))
  temperatures <- tbl(conn_temperatures, "temperatures")
  ## dynamics
  conn_dynamics <- dbConnect(RSQLite::SQLite(), paste0(experiment_folder, "dynamics.db"))
  dynamics <- tbl(conn_dynamics, "dynamics")


  ### Calculate various community level measure
  comm_cv <- get_community_CV(dynamics)
  comm_sum_rel_b_opt <- get_community_sum_rel_b_opt(temperatures, expt)

  # Pull data into memory first
  dynamics_df <- dynamics %>% collect()

  # #Compute gross asynchrony per case_id
  # async_Gross <- dynamics_df %>%
  #   group_by(case_id) %>%
  #   group_modify(~ {
  #     value <- synchrony(.x, time = "time", species = "Species_ID",
  #                        abundance = "Abundance", metric = "Gross")
  #     tibble(async_Gross = value)
  #   })
  synchrony <- dynamics_df %>%
    group_by(case_id) %>%
    group_modify(~ {
      df <- .x
      # Remove species with non-varying abundance
      varying <- df %>%
        group_by(Species_ID) %>%
        summarise(sd_ab = sd(Abundance, na.rm = TRUE), .groups = "drop") %>%
        filter(!is.na(sd_ab) & sd_ab > 0)

      df <- df %>% filter(Species_ID %in% varying$Species_ID)

      # Check that at least 2 species remain
      if (length(unique(df$Species_ID)) < 2) {
        return(tibble(synchrony = NA_real_))
      }

      value <- synchrony(df, time = "time", species = "Species_ID",
                         abundance = "Abundance", metric = "Loreau")
      tibble(synchrony = value)
    })



  # compute population stability
  # Step 1: Total abundance per case_id/time
  tot_ab_df <- dynamics_df %>%
    group_by(case_id, time) %>%
    summarise(tot_ab = sum(Abundance), .groups = "drop")

  # Step 2: Add relative abundance
  dynamics_df <- dynamics_df %>%
    left_join(tot_ab_df, by = c("case_id", "time")) %>%
    mutate(rel_ab = Abundance / tot_ab)

  # Step 3: Compute per-species mean rel. abundance and CV
  pop_df <- dynamics_df %>%
    group_by(case_id,Species_ID) %>%
    summarise(
      mean_rel_ab = mean(rel_ab, na.rm = TRUE),
      pop_CV = sd(Abundance, na.rm = TRUE) / mean(Abundance, na.rm = TRUE),
      .groups = "drop"
    )

  #Step 4: Weighted sum for community-level population variability
  pop_df <- pop_df %>%
    group_by(case_id) %>%
    summarise(
      pop_stab = sum(pop_CV * mean_rel_ab, na.rm = TRUE),
      .groups = "drop"
    )

  # # Function to calculate interaction effects
  #
  # pop_df <- pop_df %>%
  #   group_by(case_id) %>%
  #   summarise(
  #     pop_stab = sum(pop_CV * mean_rel_ab, na.rm = TRUE),
  #
  #     # Shannon diversity (H')
  #     shannon_H = -sum(mean_rel_ab * log(mean_rel_ab), na.rm = TRUE),
  #
  #     # Species richness (S)
  #     S = n(),
  #
  #     # Pielou’s Evenness
  #     evenness = ifelse(n() > 1, -sum(mean_rel_ab * log(mean_rel_ab)) / log(n()), NA_real_),
  #     rel_abundances = list(tibble(Species_ID, mean_rel_ab,mean_abundance)),
  #     .groups = "drop"
  #   )



  ## join all the community measures
  comm_measures <- expt |>
    full_join(comm_cv) |>
    full_join(comm_sum_rel_b_opt) |>
    full_join(synchrony)|>
    full_join(pop_df)


  saveRDS(comm_measures, paste0(experiment_folder, "community_measures.RDS"))

}
