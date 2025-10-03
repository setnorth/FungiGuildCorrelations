# Extract the meteorological data and average (mean) to a given grid
extract_met_data <- function(met_data, grid, column) {
  # We need to do this since the input has a slight bug with the units in the 
  # EPSG (`meters` instead of `metre`)
  crs(met_data) <- "EPSG:32633"
  
  # Project before extracting to target grid. Cumbersome way to get the CRS we
  # want to project to, but saves a parameter and is nicer to read in the 
  # pipeline.
  terra::project(met_data, 
                 sprintf("EPSG:%s", st_crs(grid)$epsg), 
                 method = "bilinear")

  # Extract
  terra::extract(met_data, terra::vect(grid), fun = mean, na.rm = TRUE, bind=TRUE) |>
    st_as_sf() |>
    st_drop_geometry() |>
    select(cell_id, all_of(column))
}