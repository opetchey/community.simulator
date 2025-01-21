
coi <- expt |> 
  filter(alpha_ij_sd == alpha_oi,
         pack == pack_oi,
         b_opt_mean == bopt_oi) |> 
  pull(case_id)

min_temperature <- min(c(temperature_treatments$temperature_control,
                         temperature_treatments$temperature_pulse))
max_temperature <- max(c(temperature_treatments$temperature_control,
                         temperature_treatments$temperature_pulse))
min_max_temps <- tibble(temperature = c(min_temperature, max_temperature),
                        y = c(0,0))
buffer <- (max_temperature - min_temperature ) / 5
temperatures <- seq(min_temperature - buffer, max_temperature + buffer, 0.1)

temp <- expt |>
  dplyr::filter(case_id == coi)
#temp$community_object
b_opt_i <- temp$community_object[[1]]$b_opt_i
a_b_i <- temp$community_object[[1]]$a_b_i
s_i <- temp$community_object[[1]]$s_i
a_d_i <- temp$community_object[[1]]$a_d_i
z_i <- temp$community_object[[1]]$z_i
#alpha_ij <- temp$community_object[[1]]$alpha_ij

## for each species do all temps
for(i in 1:length(b_opt_i)) {
  
  species_pars <- list(b_opt_i = b_opt_i[i],
                       a_b_i = a_b_i[i],
                       s_i = s_i[i],
                       a_d_i = a_d_i[i],
                       z_i = z_i[i])
  
  rates <- intrinsic_growth_rate(species_pars = species_pars,
                                 temperature = temperatures)
  
  if(i == 1)
    resp_curves <- tibble(species_id = paste0("Spp-", i),
                          temperature = temperatures,
                          intrinsic_growth_rate = rates)
  if(i > 1)
    resp_curves <- rbind(resp_curves,
                         tibble(species_id = paste0("Spp-", i),
                                temperature = temperatures,
                                intrinsic_growth_rate = rates))
  
}

#| echo: false
#| fig-width: 4
#| fig-height: 8
p_resp_curves <- resp_curves |> 
  ggplot(aes(x = temperature, y = intrinsic_growth_rate)) +
  geom_line() +
  facet_grid(rows = vars(species_id)) +
  geom_vline(xintercept = c(min_temperature, max_temperature))




## Look at some of the species level data
conn <- DBI::dbConnect(RSQLite::SQLite(), here("data/merged/dynamics.db"))
#dynamics <- tbl(conn, "dynamics")
dynamics_oi <- dbGetQuery(conn,
                          "SELECT *
           FROM dynamics
           WHERE case_id = ?",
           params = coi)
dbDisconnect(conn)


p_dynamics <- dynamics_oi |>
  select(case_id, Species_ID, Time, Treatment, Abundance) |> 
  ggplot(aes(x = Time, y = Abundance, linetype = Treatment)) +
  #geom_point() +
  geom_line() +
  facet_wrap(~ Species_ID, nrow = 3, scales = "free_y") 


#| echo: false
traits_table <- species_response_traits %>% 
  filter(case_id == coi) |>
  select(species_id,
         species_tot_deltabm_spline,
         species_tot_deltabm_raw,
         igr_pert_effect) |> 
  kbl() %>%
  kable_styling()

#| echo: false
p1 <- comm_time_stab |>
  filter(case_id == coi) |> 
  ggplot(aes(x = Time)) +
  geom_line(aes(y = Control), col = "black", linewidth = 3) +
  geom_line(aes(y = Perturbed), col = "red", linewidth = 1) +
  ylab("Total abundance")

p2 <- comm_time_stab |>
  filter(case_id == coi) |> 
  ggplot(aes(x = Time)) +
  geom_line(aes(y = comm_LRR))

p3 <- comm_time_stab |>
  filter(case_id == coi) |> 
  ggplot(aes(x = Time)) +
  geom_line(aes(y = comm_deltabm))
p_commstab1 <- p1 / p2 / p3


