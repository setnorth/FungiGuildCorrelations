extract_clc_data <- function(clc_raw, grid) {
  project(clc_raw, sprintf("EPSG:%s", st_crs(grid)$epsg), method = "near")
  
  # TODO: This is ugly, there must be a better way ------------------------------
  # The problem is that I cannot extract and sum in the grid by category since
  # that works with the `table` function in terra::extract. That function returns
  # a variable length table (depending on how many fields it encountered). So, 
  # bind won't work and I cannot attach the unique cell ID which I need for all
  # kinds of purposes. This here is now with a custom (fixed) static length table
  # function to sort the clcs into bins.
  mm   <- terra::minmax(clc_raw)
  levs <- seq.int(mm["min", 1], mm["max", 1])
  
  table_fixed <- function(v, ...) {
    v <- as.integer(v)
    v <- v[!is.na(v)]
    if (length(v) == 0) return(rep.int(0L, length(levs)))
    as.integer(tabulate(match(v, levs), nbins = length(levs)))
  }
  
  clc_count <- terra::extract(clc_raw, vect(grid),
                              fun = table_fixed,
                              method = "simple",
                              bind = TRUE,
                              na.rm = TRUE) |>  
    st_as_sf() |>
    st_drop_geometry()
  
  k <- length(levs)
  cnt_idx <- (ncol(clc_count) - k + 1):ncol(clc_count)
  names(clc_count)[cnt_idx] <- paste(levs)
  # -----------------------------------------------------------------------------
  
  # Urban: 111, 112, 121, 122, 123, 124, 131, 132, 133, 141, 142
  # Grid Codes: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11
  #
  # Agriculture: 211, 231, 242, 243
  # Grid Codes: 12, 18, 20, 21
  # 
  # Forest: 311, 312, 313
  # Grid Codes: 23, 24, 25
  #
  # Heathland, inland marches, sparse, peat bogs: 322, 324, 333, 411, 412
  # Grid Codes: 27, 29, 32, 35, 36
  #
  # Bare Rock, Glaciers, Sea and ocean, etc: 331, 332, 511, 512, 532 etc
  # Grid Codes: 30, 31, 40, 41, 44 etc
  
  # Combine columns to our categories and throw away afterwards
  clc_combined <- clc_count |>
    mutate(urban = rowSums(pick(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `10`, `11`)), .keep = "unused") |>
    mutate(agriculture = rowSums(pick(`12`, `18`, `20`, `21`)), .keep = "unused") |>
    mutate(forest = rowSums(pick(`23`, `24`, `25`)), .keep = "unused") |>
    mutate(heathland_march_sparse_bog = rowSums(pick(`27`, `29`, `33`, `35`, `36`)), .keep = "unused") |>
    mutate(bare_rock_water_diverse = rowSums(pick(matches("^\\d{2}$"))), .keep = "unused")
  
  # Calculate fractions from the categorized data
  clc_combined |>
    mutate(rowsum = rowSums(pick(-cell_id))) |>
    mutate(across(-c(cell_id,rowsum), ~ ifelse(rowsum == 0, 0, . / rowsum))) |>
    select(-rowsum)
}