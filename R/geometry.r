get_empty_grid <- function(grid_size, crs) {
  # We are cropping Svalbard. Throws a warning that can be safely ignored. We only
  # use the polygon for plotting/clipping so the attributes may be assumed
  # constant since we are not using them.
  norway_utm <- suppressWarnings(
    ne_countries(scale = 10, country = "Norway", returnclass = "sf") |>
    st_crop(xmin = 4, xmax = 31, ymin = 57, ymax = 72) |>
    st_transform(crs))
  
  # Create an empty grid, convert to sf, clip the grid to the coastline 
  # and give every cell a unique id
  st_make_grid(norway_utm,
               cellsize = grid_size,
               square = TRUE) |> 
    st_as_sf() |> 
    st_intersection(st_union(norway_utm)) |>
    mutate(cell_id = row_number())
}