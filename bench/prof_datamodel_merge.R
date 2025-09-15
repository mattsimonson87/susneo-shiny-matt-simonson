# Run from project root: source("bench/prof_datamodel_merge.R")
# Profiles the hot path and saves a profvis HTML flame graph.

# ---- setup ----
if (file.exists("DESCRIPTION") && requireNamespace("devtools", quietly = TRUE)) {
  devtools::load_all(quiet = TRUE)  # load package source into session
}
if (!file.exists("DESCRIPTION")) stop("Run from project root (where DESCRIPTION lives).")
dir.create("prof", showWarnings = FALSE)

need <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("Please install '%s' before running (install.packages('%s'))", pkg, pkg), call. = FALSE)
  }
}
need("profvis"); need("htmlwidgets"); need("tibble"); need("susneoShinyMatt")

# ---- synthetic data ----
if (file.exists("bench/make_big_sample.R")) {
  source("bench/make_big_sample.R")
} else {
  make_big_sample <- function(n = 2e5, n_sites = 20, n_types = 8, start = as.Date("2024-01-01")) {
    sites <- sprintf("SITE%03d", seq_len(n_sites))
    types <- c("Electricity","Gas","Steam","WATER","SOLAR","Wind","Diesel","Other")[seq_len(n_types)]
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
}

# ---- profile ----
set.seed(42)

# Use :: if DataModel is exported; if not, switch to :::.
dm  <- susneoShinyMatt::DataModel$new()
dat <- make_big_sample(2e5)

pv <- profvis::profvis({
  dm$reset(clear_sources = TRUE)
  dat_c <- dm$canonicalize(dat)
  dm$validate(dat_c)        # returns TRUE or list(errors = ...)
  dm$merge(dat_c)           # no mode= arg in your current API
})

out <- sprintf("prof/profvis-merge-%s.html", format(Sys.time(), "%Y%m%d-%H%M%S"))
htmlwidgets::saveWidget(pv, out, selfcontained = TRUE)
message("Profile saved: ", out)
