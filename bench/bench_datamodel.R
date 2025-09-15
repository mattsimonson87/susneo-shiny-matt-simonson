# Run from project root: source("bench/bench_datamodel.R")
# Benchmarks DataModel hotspots: canonicalize → validate → merge (append/override)

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
need("bench"); need("readr"); need("tibble"); need("susneoShinyMatt")

# ---- synthetic data ----
if (file.exists("bench/make_big_sample.R")) {
  source("bench/make_big_sample.R")
} else {
  make_big_sample <- function(n = 1e5, n_sites = 20, n_types = 8, start = as.Date("2024-01-01")) {
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

# ---- parameters ----
n <- as.integer(Sys.getenv("BENCH_N", "200000"))  # override with: Sys.setenv(BENCH_N="500000")
set.seed(42)
dm <- susneoShinyMatt:::DataModel$new()
dat <- make_big_sample(n)

# ---- benchmark ----
# prep canonicalized data once
dat_c <- dm$canonicalize(dat)

res <- bench::mark(
  canonicalize = dm$canonicalize(dat),
  validate     = dm$validate(dat_c),
  
  # Append case: merge into an empty model
  merge_append = {
    dm1 <- susneoShinyMatt::DataModel$new()
    dm1$reset(clear_sources = TRUE)
    dm1$merge(dat_c)
  },
  
  # Override case: merge same data twice (second call replaces same IDs)
  merge_override = {
    dm2 <- susneoShinyMatt::DataModel$new()
    dm2$reset(clear_sources = TRUE)
    dm2$merge(dat_c)  # first append
    dm2$merge(dat_c)  # second = override
  },
  
  iterations = 3,
  check = FALSE,
  memory = TRUE,
  filter_gc = TRUE
)
print(res)
out <- sprintf("prof/bench-%s-%s.csv", format(n, big.mark = ""), format(Sys.time(), "%Y%m%d-%H%M%S"))
readr::write_csv(as.data.frame(res), out)
message("Saved: ", out)
