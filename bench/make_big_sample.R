make_big_sample <- function(n = 1e5, n_sites = 20, n_types = 8, start = as.Date("2024-01-01")) {
  sites <- sprintf("SITE%03d", seq_len(n_sites))
  types <- c("Electricity", "Gas", "Steam", "WATER", "SOLAR", "Wind", "Diesel", "Other")[seq_len(n_types)]
  dates <- start + sample.int(365, n, replace = TRUE)
  tibble::tibble(
    id   = sprintf("ID-%07d", seq_len(n)),
    site = sample(sites, n, TRUE),
    type = sample(types, n, TRUE),
    date = dates,
    value = round(rexp(n, rate = 1/50), 2),
    carbon_emission_kgco2e = round(value * runif(n, 0.3, 1.2), 2),
    source = "bench-gen"
  )
}