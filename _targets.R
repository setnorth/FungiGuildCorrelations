# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
library(crew) # Parallel processing
library(geotargets) # Otherwise errors when loading rasters

library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)

# Global variables
g_grid_size = 10000     # The size of the grid in m
g_min_obs_per_cell = 50 # The minimum amount of observations per cell we want to include
g_crs = 25833           # The projection we are working with


tar_option_set(
  packages = c("tidyverse", "jsonlite", "rnaturalearth", "sf", "vegan",
               "terra", "betareg", "marginaleffects"), 
  
  controller = crew_controller_local(
    workers = max(1, parallel::detectCores() - 1),  # how many parallel jobs
    seconds_idle = 60
  )
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source("other_functions.R") # Source other scripts as needed.

tar_plan(
  # -- Files
  
  # === Gbif occurence data ===
  # The data was obtained via the 'Get dataset' screen at http://gbif.org 
  # with the following parameters:
  #   1. Occurence status: Present
  #   2. Year: 1990-2020
  #   3. Country or area: Norway
  #   4. Dataset: Artsdatabanken
  tar_target(gbif_file, "data/artsdatabanken_0016789-250827131500795.csv", format = "file"),
  
  # === FUNGuild data ===
  # The json file containing the FUNGuild database was obtained via
  # http://stbates.org/funguild_db.php. Saved as a file it had to be cleaned since
  # it contained html headers and footers.
  tar_target(funguild_file, "data/funguild_db.json", format = "file"),
  
  # === Meteorological data ===
  # Datasource: https://github.com/metno/seNorge_docs/wiki/seNorge_2018
  tar_target(precip_file, "data/seNorge2018_rr_normal_1991_2020_yearly.nc", format = "file"),
  tar_target(tnorm_file, "data/seNorge2018_tg_normal_1991_2020_yearly.nc", format = "file"),
  
  # === Corine Landcover data ===
  # Source: https://land.copernicus.eu/en/products/corine-land-cover/clc2018
  # Check CLC Legend in CSV file in information folder
  tar_target(clc_file, "data/corine/U2018_CLC2018_V2020_20u1.tif", format = "file"),
  
  # -- Reading
  gbif_raw = read_raw_gbif_data(gbif_file),
  funguild_raw = read_raw_funguild_data(funguild_file),
  tar_terra_rast(precip_raw, terra::rast(precip_file)), # Throws GDAL warnings
  tar_terra_rast(tnorm_raw, terra::rast(tnorm_file)), # Throws GDAL warnings
  tar_terra_rast(clc_raw, terra::rast(clc_file)), # Throws GDAL warnings
  
  # -- Cleaning / Pre-processing
  gbif_clean = clean_gbif_raw(gbif_raw),
  funguild_simplified = simplify_funguild_raw(funguild_raw),
  precip_cells = extract_met_data(precip_raw, empty_grid, "rr"), # Throws GDAL warnings
  tnorm_cells = extract_met_data(tnorm_raw, empty_grid, "tg"), # Throws GDAL warnings
  clc_cells = extract_clc_data(clc_raw, empty_grid), # Throws GDAL warnings

  # -- Combining
  gbif_augmented = augment_gbif_by_funguild(gbif_clean, funguild_simplified),
  gbif_gridded = grid_gbif(gbif_augmented, empty_grid),
  # Remove all cells from the gridded gbif that contain less than 
  # g_min_obs_per_cell observations
  gbif_gridded_pruned = prune_gbif_grid(gbif_gridded, g_min_obs_per_cell),
  
  # -- Utility
  empty_grid = get_empty_grid(g_grid_size, g_crs),
    
  # -- Computing (Rarefaction etc.)
  genus_community_matrix = get_genus_community_matrix(gbif_gridded_pruned),
  alpha_diversity_cells = get_alpha_diversity_cells(genus_community_matrix, g_min_obs_per_cell),
  guild_fraction_cells = get_guild_fraction_cells(gbif_gridded_pruned),
  data_grid = combine_data(empty_grid,
                           guild_fraction_cells,
                           alpha_diversity_cells,
                           precip_cells,
                           tnorm_cells,
                           clc_cells),
  guild_count_pruned = {gbif_gridded_pruned |> st_drop_geometry() |> count(guild)},
  
  # -- Plots
  # Map that shows the number of observations after pruning for minimum observations
  valid_obs_plot = get_valid_obs_plot(gbif_gridded_pruned),
  alpha_diversity_plot = get_alpha_diversity_plot(data_grid),
  guild_distribution_plot = get_guild_distribution_plot(data_grid),
  
  # -- Statistics
  m_saprotroph = get_beta_regression(data_grid, 
                                     "Saprotroph", # Dependent
                                     c("Lichenized", 
                                       "Ectomycorrhizal", 
                                       "bare_rock_water_diverse"), # covariates
                                     g_min_obs_per_cell),
  m_lichenized = get_beta_regression(data_grid, 
                                     "Lichenized", # Dependent
                                     c("Saprotroph", 
                                       "Ectomycorrhizal", 
                                       "bare_rock_water_diverse"), # covariates
                                     g_min_obs_per_cell),
  m_ectomycorrhizal = get_beta_regression(data_grid, 
                                          "Ectomycorrhizal", # Dependent
                                          c("Saprotroph", 
                                            "Lichenized", 
                                            "bare_rock_water_diverse"), # covariates
                                          g_min_obs_per_cell),
  avg_slopes_saprotroph = avg_slopes(m_saprotroph),
  avg_slopes_lichenized = avg_slopes(m_lichenized),
  avg_slopes_ectomycorrhizal = avg_slopes(m_ectomycorrhizal)
)
