# Read a gbif file
# Immediately after reading we dismiss rows that:
#   1. Contain no species
#   2. Contain no genus
#   3. Contain no latitude or longitude
read_raw_gbif_data <- function(fname){
  read_delim(fname,
             col_select = c(species,
                            genus,
                            decimalLatitude,
                            decimalLongitude,
                            coordinateUncertaintyInMeters),
             show_col_types = FALSE) |>
    filter(!is.na(species),
           !is.na(genus), 
           !is.na(decimalLatitude),
           !is.na(decimalLongitude))
}

# Not much to do here, we excluded most while reading. Kept in case that changes.
clean_gbif_raw <- function(gbif_raw){
  gbif_raw
}

# Augment the gbif data with the simplified funguild data
augment_gbif_by_funguild <- function(gbif_data, funguild_data){
  inner_join(gbif_data,
             funguild_data,
             by=c("genus" = "taxon"))
}

# Create a grid from the gbif data containing only cells that contain a minimum
# number of observations
grid_gbif <- function(augmented_gbif, grid){
  # Convert data to ETRS89 / UTM 33N simple features object
  data_utm <- st_as_sf(augmented_gbif,
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326,
           remove = FALSE) |> 
    st_transform(st_crs(grid))
  
  st_join(grid, 
          data_utm, 
          join = st_contains, 
          left = FALSE)
}

# Remove all cells from the gridded gbif that contain less than 
# g_min_obs_per_cell observations
prune_gbif_grid <- function(gridded_data, g_min_obs_per_cell){
  # Count the the number of observations per cell and retain if >= g_min_obs_per_cell
  valid_obs_per_cell <- count(st_drop_geometry(gridded_data),
                              cell_id,
                              name = "n_obs") |>
    filter(n_obs >= g_min_obs_per_cell)
  
  # Remoe from gridded_data cells that contain less than g_min_obs_per_cell observations
  gridded_data |> filter(cell_id %in% valid_obs_per_cell$cell_id)
}