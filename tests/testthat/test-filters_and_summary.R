# test-filters_and_summary.R
# Tests for DataModel filtering and summary functions

# Helper function to create a test DataModel with controlled data
create_test_dm <- function(test_data) {
  dm <- DataModel$new()
  
  # Replace dataset and update filters to match
  dm$dataset <- test_data
  
  # Recalculate filters based on new dataset
  if (!is.null(test_data) && nrow(test_data) > 0) {
    rng <- range(test_data$date, na.rm = TRUE)
    dm$filters <- list(
      date  = rng,
      sites = sort(unique(test_data$site)),
      types = sort(unique(test_data$type))
    )
  }
  
  return(dm)
}

test_that("DataModel initializes with sample data", {
  dm <- DataModel$new()
  
  expect_s3_class(dm, "DataModel")
  expect_s3_class(dm, "R6")
  expect_true(is.data.frame(dm$dataset))
  expect_gt(nrow(dm$dataset), 0)
  expect_true(all(c("id", "site", "date", "type", "value", "carbon_emission_kgco2e") %in% names(dm$dataset)))
})

test_that("canonical_names function works correctly", {
  # Test the canonical_names helper function
  expect_equal(canonical_names("facility_id"), "id")
  expect_equal(canonical_names("site_id"), "site")
  expect_equal(canonical_names("reading_date"), "date")
  expect_equal(canonical_names("energy_type"), "type")
  expect_equal(canonical_names("consumption"), "value")
  expect_equal(canonical_names("emissions"), "carbon_emission_kgco2e")
  expect_equal(canonical_names("emission_kgco2e"), "carbon_emission_kgco2e")
  expect_equal(canonical_names("some_kgco2e_field"), "carbon_emission_kgco2e")
})

test_that("set_filters updates filter values correctly", {
  dm <- DataModel$new()
  
  # Get initial filter values
  initial_dates <- dm$filters$date
  initial_sites <- dm$filters$sites
  initial_types <- dm$filters$types
  
  # Test date filter update
  date_range <- c(as.Date("2024-01-01"), as.Date("2024-12-31"))
  dm$set_filters(dates = date_range)
  expect_equal(dm$filters$date, date_range)
  
  # Test site filter update (use actual sites from sample data if available)
  if (length(initial_sites) >= 2) {
    sites <- initial_sites[1:2]
    dm$set_filters(sites = sites)
    expect_equal(dm$filters$sites, sites)
  }
  
  # Test type filter update (use actual types from sample data if available)
  if (length(initial_types) >= 1) {
    types <- initial_types[1]
    dm$set_filters(types = types)
    expect_equal(dm$filters$types, types)
  }
  
  # Test multiple filters at once
  dm$set_filters(
    dates = date_range, 
    sites = initial_sites[1], 
    types = initial_types[1]
  )
  expect_equal(dm$filters$date, date_range)
  expect_equal(dm$filters$sites, initial_sites[1])
  expect_equal(dm$filters$types, initial_types[1])
})

test_that("filtered_data applies date filter correctly", {
  # Create test data with known dates
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = c("SITE1", "SITE1", "SITE2", "SITE2"),
    date = as.Date(c("2024-01-15", "2024-02-15", "2024-03-15", "2024-04-15")),
    type = c("Electric", "Gas", "Electric", "Gas"),
    value = c(100, 200, 300, 400),
    carbon_emission_kgco2e = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Filter to February-March
  dm$set_filters(dates = c(as.Date("2024-02-01"), as.Date("2024-03-31")))
  filtered <- dm$filtered_data()
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$date >= as.Date("2024-02-01")))
  expect_true(all(filtered$date <= as.Date("2024-03-31")))
})

test_that("filtered_data applies site filter correctly", {
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = c("SITE1", "SITE1", "SITE2", "SITE3"),
    date = as.Date(c("2024-01-15", "2024-02-15", "2024-03-15", "2024-04-15")),
    type = c("Electric", "Gas", "Electric", "Gas"),
    value = c(100, 200, 300, 400),
    carbon_emission_kgco2e = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Filter to SITE1 and SITE3
  dm$set_filters(sites = c("SITE1", "SITE3"))
  filtered <- dm$filtered_data()
  
  expect_equal(nrow(filtered), 3)
  expect_true(all(filtered$site %in% c("SITE1", "SITE3")))
})

test_that("filtered_data applies type filter correctly", {
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = c("SITE1", "SITE1", "SITE2", "SITE2"),
    date = as.Date(c("2024-01-15", "2024-02-15", "2024-03-15", "2024-04-15")),
    type = c("Electric", "Gas", "Electric", "Water"),
    value = c(100, 200, 300, 400),
    carbon_emission_kgco2e = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Filter to Electric only
  dm$set_filters(types = "Electric")
  filtered <- dm$filtered_data()
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$type == "Electric"))
})

test_that("filtered_data handles multiple filters simultaneously", {
  test_data <- data.frame(
    id = as.character(1:10),
    site = rep(c("SITE1", "SITE2"), 5),
    date = seq(as.Date("2024-01-01"), as.Date("2024-01-10"), by = "day"),
    type = rep(c("Electric", "Gas"), each = 5),
    value = 100:109,
    carbon_emission_kgco2e = 10:19,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Apply all filters
  dm$set_filters(
    dates = c(as.Date("2024-01-03"), as.Date("2024-01-07")),
    sites = "SITE1",
    types = "Electric"
  )
  filtered <- dm$filtered_data()
  
  expect_equal(nrow(filtered), 2)
  expect_true(all(filtered$site == "SITE1"))
  expect_true(all(filtered$type == "Electric"))
  expect_true(all(filtered$date >= as.Date("2024-01-03")))
  expect_true(all(filtered$date <= as.Date("2024-01-07")))
})

test_that("filtered_data handles NA dates gracefully", {
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = c("SITE1", "SITE1", "SITE1"),
    date = as.Date(c("2024-01-15", NA, "2024-03-15")),
    type = c("Electric", "Gas", "Electric"),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  dm$set_filters(dates = c(as.Date("2024-01-01"), as.Date("2024-12-31")))
  filtered <- dm$filtered_data()
  
  # NA dates should be excluded
  expect_equal(nrow(filtered), 2)
  expect_false(any(is.na(filtered$date)))
})

test_that("KPI calculations work correctly", {
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = c("SITE1", "SITE1", "SITE2", "SITE2"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")),
    type = c("Electric", "Gas", "Electric", "Gas"),
    value = c(100, 200, 300, 400),
    carbon_emission_kgco2e = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Test total consumption
  expect_equal(dm$kpi_total_consumption(), 1000)
  
  # Test total emissions
  expect_equal(dm$kpi_total_emissions(), 100)
  
  # Test average daily consumption
  expect_equal(dm$kpi_avg_daily_consumption(), 250)  # 1000 / 4 days
  
  # Test energy intensity (per site-day)
  expect_equal(dm$kpi_energy_intensity(), 250)  # 1000 / 4 site-days
})

test_that("KPI calculations respect filters", {
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = c("SITE1", "SITE1", "SITE2", "SITE2"),
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")),
    type = c("Electric", "Gas", "Electric", "Gas"),
    value = c(100, 200, 300, 400),
    carbon_emission_kgco2e = c(10, 20, 30, 40),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Filter to SITE1 only
  dm$set_filters(sites = "SITE1")
  
  expect_equal(dm$kpi_total_consumption(), 300)
  expect_equal(dm$kpi_total_emissions(), 30)
})

test_that("timeseries aggregation works correctly", {
  test_data <- data.frame(
    id = as.character(1:6),
    site = rep("SITE1", 6),
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", 
                     "2024-02-01", "2024-02-02", "2024-03-01")),
    type = rep("Electric", 6),
    value = c(100, 50, 200, 300, 100, 150),
    carbon_emission_kgco2e = c(10, 5, 20, 30, 10, 15),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Test daily aggregation
  ts_day <- dm$timeseries(by = "day")
  expect_equal(nrow(ts_day), 5)
  expect_equal(ts_day$value[ts_day$period == as.Date("2024-01-01")], 150)
  
  # Test monthly aggregation
  ts_month <- dm$timeseries(by = "month")
  expect_equal(nrow(ts_month), 3)
  expect_equal(ts_month$value[ts_month$period == as.Date("2024-01-01")], 350)
  expect_equal(ts_month$value[ts_month$period == as.Date("2024-02-01")], 400)
})

test_that("compare function works correctly", {
  test_data <- data.frame(
    id = as.character(1:6),
    site = rep(c("SITE1", "SITE2", "SITE3"), each = 2),
    date = rep(as.Date("2024-01-01"), 6),
    type = rep(c("Electric", "Gas"), 3),
    value = c(100, 200, 300, 400, 500, 600),
    carbon_emission_kgco2e = c(10, 20, 30, 40, 50, 60),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Compare by site
  cmp_site <- dm$compare(by = "site")
  expect_equal(nrow(cmp_site), 3)
  expect_equal(cmp_site$site[1], "SITE3")  # Highest total
  expect_equal(cmp_site$value[1], 1100)
  
  # Compare by type
  cmp_type <- dm$compare(by = "type")
  expect_equal(nrow(cmp_type), 2)
  expect_equal(cmp_type$type[1], "Gas")  # Higher total
  expect_equal(cmp_type$value[1], 1200)
})

test_that("status function returns correct information", {
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = c("SITE1", "SITE2", "SITE3"),
    date = as.Date(c("2024-01-01", "2024-06-15", "2024-12-31")),
    type = c("Electric", "Gas", "Water"),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  status <- dm$status()
  
  expect_equal(status$n_rows, 3)
  expect_equal(status$n_sites, 3)
  expect_equal(status$date_min, as.Date("2024-01-01"))
  expect_equal(status$date_max, as.Date("2024-12-31"))
  expect_equal(sort(status$sites), c("SITE1", "SITE2", "SITE3"))
  expect_equal(sort(status$types), c("Electric", "Gas", "Water"))
  # Don't test sources_count as it depends on initialization
})

test_that("summary_table returns filtered data", {
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = c("SITE1", "SITE2", "SITE1"),
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    type = c("Electric", "Gas", "Electric"),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # summary_table should return the same as filtered_data
  dm$set_filters(sites = "SITE1")
  summary <- dm$summary_table()
  filtered <- dm$filtered_data()
  
  expect_identical(summary, filtered)
})

test_that("filtered_data returns empty data frame when no data matches filters", {
  test_data <- data.frame(
    id = c("1", "2"),
    site = c("SITE1", "SITE2"),
    date = as.Date(c("2024-01-01", "2024-02-01")),
    type = c("Electric", "Gas"),
    value = c(100, 200),
    carbon_emission_kgco2e = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Filter to non-existent site
  dm$set_filters(sites = "NONEXISTENT")
  filtered <- dm$filtered_data()
  
  expect_equal(nrow(filtered), 0)
  expect_true(is.data.frame(filtered))
})

test_that("KPIs handle empty filtered data gracefully", {
  test_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm(test_data)
  
  # Filter to non-existent site
  dm$set_filters(sites = "NONEXISTENT")
  
  expect_equal(dm$kpi_total_consumption(), 0)
  expect_equal(dm$kpi_total_emissions(), 0)
  expect_equal(dm$kpi_avg_daily_consumption(), 0)
  expect_equal(dm$kpi_energy_intensity(), 0)
})