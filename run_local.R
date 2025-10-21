# Run the app locally without installing the package (no Rtools needed)
# Load environment variables from .env file
if (file.exists(".env")) {
  dotenv::load_dot_env(".env")
}
if (!requireNamespace("pkgload", quietly = TRUE)) install.packages("pkgload")
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")
}

# Install runtime deps as binaries from CRAN (fast; no build tools)
pak::pak(c(
  "shiny","bslib","plotly","DT","R6","dplyr","tidyr","readr","readxl","lubridate","stringr"
))

# Load the package source in-place and run the app
pkgload::load_all(".")
susneoShinyMatt::run_app()