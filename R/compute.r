get_genus_community_matrix <- function(gbif_gridded_pruned){
  gbif_gridded_pruned |>
    st_drop_geometry() |> 
    count(cell_id, genus, name = "abundance") |> 
    pivot_wider(names_from = genus, values_from = abundance, values_fill = 0) |>
    column_to_rownames("cell_id")
}

get_alpha_diversity_cells <- function(community_matrix, g_min_obs_per_cell){
  community_matrix |>
    vegan::rarefy(sample = g_min_obs_per_cell) |>
    enframe(name = "cell_id", value = "alpha_diversity") |>
    mutate(cell_id = as.integer(cell_id))
}

get_guild_fraction_cells <- function(gbif_gridded_pruned){
  # Take the gridded data and:
  # 1. drop the geometry (we don't need it for calculating guild distributions)
  # 2. count for every cell_id the number of times a genus appears
  # 3. create a guild matrix that by pivoting the result
  # 4. Preserve the cell_id as the rownames.
  guild_matrix <- gbif_gridded_pruned |> 
    st_drop_geometry() |> 
    count(cell_id, guild, name = "guild_abundance") |> 
    pivot_wider(names_from = guild, 
                values_from = guild_abundance, 
                values_fill = 0) |>
    column_to_rownames("cell_id")
  
  # Simplify, i.e., sum up Saprotrophs. Remove also
  # Epiphyte, Endophyte, Pathogens and Parasites. 
  # They have almost no observations.
  guild_matrix_simplified <- guild_matrix |>
    mutate(Saprotroph = rowSums(pick(contains("Saprotroph"))),) |>
    select(-contains("Saprotroph"), 
           -contains("Pathogen"), 
           -contains("Parasite"),
           -Epiphyte,
           -Endophyte,
           Saprotroph)
  
  # Using decostand to standardise
  # TODO: Argument about that and detail hellinger (or total if used)
  # Result is the expected standardized [0,1] distribution of the guilds
  guild_matrix_simplified |>
    decostand(method = "total") |>
    rownames_to_column("cell_id") |>
    mutate(cell_id = as.integer(cell_id))
}

# Combine all data sources together via the cell_id
combine_data <- function(...){
  reduce(list(...), inner_join, by = "cell_id") |>
    rename(precip_mm = rr) |>
    rename(tmean_c = tg)
}