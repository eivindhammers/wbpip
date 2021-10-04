empty_gd_compute_pip_stats_response <- list(
  gini = NA_real_,
  median = NA_real_,
  rmhalf = NA_real_,
  polarization = NA_real_,
  ris = NA_real_,
  mld = NA_real_,
  dcm = NA_real_,
  deciles = rep(NA_real_, 10), # Potential issue here as I am hard coding deciles, when the function can theoretically return any quantiles
  headcount = NA_real_,
  poverty_gap = NA_real_,
  poverty_severity = NA_real_,
  eh = NA_real_,
  epg = NA_real_,
  ep = NA_real_,
  gh = NA_real_,
  gpg = NA_real_,
  gp = NA_real_,
  watts = NA_real_,
  dl = NA_real_,
  ddl = NA_real_,
  is_normal = FALSE,
  is_valid = FALSE
)

usethis::use_data(empty_gd_compute_pip_stats_response,
  overwrite = TRUE,
  internal  = TRUE
)
