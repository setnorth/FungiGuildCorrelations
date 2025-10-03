get_beta_regression <- function(data, response, covariates, g_min_obs_per_cell){
  # Get rid of numerical instability, center & scale! Use max values as 1 values,
  # the rest is then fractional
  # Normalizer helper (get everything into the [0,1] interval)
  rng01 <- \(x) (x - min(x, na.rm=TRUE)) / (max(x, na.rm=TRUE) - min(x, na.rm=TRUE))
  dd <- data |>
    st_drop_geometry() |>
    mutate(precip_mm = rng01(precip_mm)) |>
    mutate(tmean_c = rng01(tmean_c)) |>
    mutate(alpha_diversity = alpha_diversity/g_min_obs_per_cell) |>
    select(-all_of(covariates), -cell_id)
  
  f <- reformulate(setdiff(names(dd), response), response) 
  betareg(f, data = dd, link = "logit")
}