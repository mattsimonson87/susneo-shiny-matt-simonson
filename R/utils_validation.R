canonical_names <- function(nm) {
  nm <- tolower(trimws(gsub("[^a-z0-9]+", "_", nm)))
  map <- c(
    facility_id="id", record_id="id", row_id="id", meter_id="id", id="id",
    site="site", site_id="site", facility="site", facility_code="site", building="site", location="site",
    date="date", day="date", month="date", reading_date="date", period_start="date", start_date="date",
    type="type", energy_type="type", utility="type", fuel="type",
    value="value", consumption="value", usage="value", quantity="value", amount="value",
    carbon_emission_kgco2e="carbon_emission_kgco2e", emissions="carbon_emission_kgco2e",
    emission_kgco2e="carbon_emission_kgco2e"
  )
  std <- ifelse(nm %in% names(map), unname(map[nm]), nm)
  std[grepl("kgco2e|co2e", std)] <- "carbon_emission_kgco2e"  # catch-all
  unname(std)
}