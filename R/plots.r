get_valid_obs_plot <- function(dd_gbif){
  # Count observations per cell
  obs_per_cell <- dd_gbif |> 
    group_by(cell_id) |>
    summarise(n_obs=n(), .groups = "drop")
  
  ggplot(obs_per_cell) + 
    geom_sf(aes(fill = n_obs), color = NA) +
    scale_fill_viridis_c(name = "Observations / cell", 
                         trans = "log10", 
                         labels = scales::label_number())
}

get_alpha_diversity_plot <- function(data_grid){
  ggplot(data_grid) + 
    geom_sf(aes(fill = alpha_diversity), color = NA) +
    scale_fill_viridis_c(name = "Alpha Diversity", labels = scales::label_number())
}

get_guild_distribution_plot <- function(data_grid){
  guild_distributions_longer <- data_grid |>
    select(cell_id, Ectomycorrhizal, Lichenized, Saprotroph) |>
    pivot_longer(c(Ectomycorrhizal, Lichenized, Saprotroph), 
                 names_to = "guild", 
                 values_to = "fraction")
  
  ggplot(guild_distributions_longer) + 
    geom_sf(aes(fill = fraction), color = NA) +
    scale_fill_viridis_c(name = expression(sqrt(fraction)),
                         limits = c(0,1),
                         trans = "sqrt",
                         labels = scales::label_number()) +
    facet_wrap(vars(guild))
}