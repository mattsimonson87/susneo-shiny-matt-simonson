# test-validation.R
# Tests for DataModel validation and canonicalization functionality

# Ensure canonical_names function is available
if (!exists("canonical_names")) {
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
}

test_that("canonical_names function works correctly", {
  # Test various column name mappings
  expect_equal(canonical_names("facility_id"), "id")
  expect_equal(canonical_names("site_id"), "site")
  expect_equal(canonical_names("reading_date"), "date")
  expect_equal(canonical_names("energy_type"), "type")
  expect_equal(canonical_names("consumption"), "value")
  expect_equal(canonical_names("emissions"), "carbon_emission_kgco2e")
  expect_equal(canonical_names("emission_kgco2e"), "carbon_emission_kgco2e")
  expect_equal(canonical_names("some_kgco2e_field"), "carbon_emission_kgco2e")
  expect_equal(canonical_names("unknown_field"), "unknown_field")
})

test_that("canonicalize processes data with canonical column names", {
  dm <- DataModel$new()
  
  # Test with already canonical column names
  test_data <- data.frame(
    id = "1",
    site = "site1",
    date = "2024-01-01",
    type = "electric",
    value = "100",
    carbon_emission_kgco2e = "10",
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  # Columns should remain the same since they're already canonical
  expect_true(all(c("id", "site", "date", "type", "value", "carbon_emission_kgco2e") %in% names(result)))
  
  # Check data transformations
  expect_equal(result$site, "SITE1")  # Uppercase
  expect_equal(result$type, "Electric")  # Title case
  expect_true(inherits(result$date, "Date"))  # Parsed as Date
  expect_equal(result$value, 100)  # Numeric
  expect_equal(result$carbon_emission_kgco2e, 10)  # Numeric
})

test_that("canonicalize standardizes ID format", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("  123  ", "456.00", "789.0", "ABC123", "  DEF  456  "),
    site = rep("SITE1", 5),
    date = rep("2024-01-01", 5),
    type = rep("Electric", 5),
    value = rep("100", 5),
    carbon_emission_kgco2e = rep("10", 5),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  # IDs should be trimmed and decimal zeros removed
  expect_equal(result$id[1], "123")
  expect_equal(result$id[2], "456")
  expect_equal(result$id[3], "789")
  expect_equal(result$id[4], "ABC123")
  expect_equal(result$id[5], "DEF 456")  # Internal spaces collapsed
})

test_that("canonicalize converts site to uppercase", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = c("site1", "Site2", "SITE3"),
    date = rep("2024-01-01", 3),
    type = rep("Electric", 3),
    value = rep("100", 3),
    carbon_emission_kgco2e = rep("10", 3),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  expect_equal(result$site[1], "SITE1")
  expect_equal(result$site[2], "SITE2")
  expect_equal(result$site[3], "SITE3")
})

test_that("canonicalize handles type field with Title Case", {
  dm <- DataModel$new()
  
  # Test with lowercase input (should convert to Title Case)
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = rep("SITE1", 3),
    date = rep("2024-01-01", 3),
    type = c("electric", "natural gas", "water supply"),
    value = rep("100", 3),
    carbon_emission_kgco2e = rep("10", 3),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  expect_equal(result$type[1], "Electric")
  expect_equal(result$type[2], "Natural Gas")
  expect_equal(result$type[3], "Water Supply")
  
  # Test with uppercase input
  # Note: tools::toTitleCase doesn't lowercase all-caps strings first
  test_data2 <- data.frame(
    id = c("4", "5"),
    site = rep("SITE1", 2),
    date = rep("2024-01-01", 2),
    type = c("NATURAL GAS", "SOLAR PANEL"),
    value = rep("100", 2),
    carbon_emission_kgco2e = rep("10", 2),
    stringsAsFactors = FALSE
  )
  
  result2 <- dm$canonicalize(test_data2)
  
  # tools::toTitleCase preserves all-caps
  expect_equal(result2$type[1], "NATURAL GAS")
  expect_equal(result2$type[2], "SOLAR PANEL")
})

test_that("canonicalize parses various date formats", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = as.character(1:10),
    site = rep("SITE1", 10),
    date = c(
      "2024-01-15",      # ISO format
      "15-01-2024",      # DD-MM-YYYY
      "01/15/2024",      # MM/DD/YYYY
      "15/01/2024",      # DD/MM/YYYY
      "15-Jan-2024",     # DD-MMM-YYYY
      "Jan 15, 2024",    # MMM DD, YYYY
      "15 Jan 2024",     # DD MMM YYYY
      "2024/01/15",      # YYYY/MM/DD
      "44941",           # Excel serial (2023-01-15)
      "20-08-2025"       # DD-MM-YYYY (from comment)
    ),
    type = rep("Electric", 10),
    value = rep("100", 10),
    carbon_emission_kgco2e = rep("10", 10),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  # All should be parsed as Date objects
  expect_true(all(inherits(result$date, "Date")))
  
  # Check specific conversions
  expect_equal(result$date[1], as.Date("2024-01-15"))
  expect_equal(result$date[2], as.Date("2024-01-15"))
  expect_equal(result$date[3], as.Date("2024-01-15"))
  expect_equal(result$date[9], as.Date("2023-01-15"))
  expect_equal(result$date[10], as.Date("2025-08-20"))
})

test_that("canonicalize handles Excel date serials", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = rep("SITE1", 3),
    date = c("44927", "44927.0", "1"),  # Excel dates
    type = rep("Electric", 3),
    value = rep("100", 3),
    carbon_emission_kgco2e = rep("10", 3),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  # 44927 is 2023-01-01 in Excel 1900 system
  expect_equal(result$date[1], as.Date("2023-01-01"))
  expect_equal(result$date[2], as.Date("2023-01-01"))
  # 1 in Excel could be either 1900-01-01 (1900 system) or 1904-01-02 (1904 system)
  # The implementation uses 1904 system as fallback for small numbers
  expect_equal(result$date[3], as.Date("1904-01-02"))
})

test_that("canonicalize converts numeric columns", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = rep("SITE1", 4),
    date = rep("2024-01-01", 4),
    type = rep("Electric", 4),
    value = c("100", "200.5", "not_a_number", "300"),
    carbon_emission_kgco2e = c("10.5", "20", "invalid", "30.75"),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  expect_equal(result$value[1], 100)
  expect_equal(result$value[2], 200.5)
  expect_true(is.na(result$value[3]))
  expect_equal(result$value[4], 300)
  
  expect_equal(result$carbon_emission_kgco2e[1], 10.5)
  expect_equal(result$carbon_emission_kgco2e[2], 20)
  expect_true(is.na(result$carbon_emission_kgco2e[3]))
  expect_equal(result$carbon_emission_kgco2e[4], 30.75)
})

test_that("validate detects missing required columns", {
  dm <- DataModel$new()
  
  # Missing 'value' column
  test_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  expect_true("errors" %in% names(result))
  expect_true(any(grepl("Missing required columns.*value", result$errors)))
})

test_that("validate detects multiple missing columns", {
  dm <- DataModel$new()
  
  # Missing multiple columns
  test_data <- data.frame(
    id = "1",
    site = "SITE1",
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  expect_true(any(grepl("Missing required columns", result$errors)))
  expect_true(any(grepl("date", result$errors)))
  expect_true(any(grepl("type", result$errors)))
  expect_true(any(grepl("value", result$errors)))
  expect_true(any(grepl("carbon_emission_kgco2e", result$errors)))
})

test_that("validate detects unparseable dates", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = rep("SITE1", 3),
    date = as.Date(c("2024-01-01", NA, "2024-03-01")),
    type = rep("Electric", 3),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  expect_true(any(grepl("Unparseable dates.*1 row", result$errors)))
})

test_that("validate detects negative values", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "3", "4"),
    site = rep("SITE1", 4),
    date = as.Date(rep("2024-01-01", 4)),
    type = rep("Electric", 4),
    value = c(100, -200, 300, -400),
    carbon_emission_kgco2e = c(10, 20, -30, -40),
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  # Should report negative values in both columns
  expect_true(any(grepl("Negative values in 'value'.*2 row", result$errors)))
  expect_true(any(grepl("Negative values in 'carbon_emission_kgco2e'.*2 row", result$errors)))
})

test_that("validate detects duplicate IDs", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "2", "3", "3", "3"),
    site = rep("SITE1", 6),
    date = as.Date(rep("2024-01-01", 6)),
    type = rep("Electric", 6),
    value = rep(100, 6),
    carbon_emission_kgco2e = rep(10, 6),
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  expect_true(any(grepl("Duplicate 'id' values.*2 unique.*3 extra row", result$errors)))
  expect_true(any(grepl("Merge will keep last occurrence", result$errors)))
})

test_that("validate returns TRUE for valid data", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("1", "2", "3"),
    site = c("SITE1", "SITE2", "SITE3"),
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    type = c("Electric", "Gas", "Water"),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(isTRUE(result))
})

test_that("validate handles edge cases", {
  dm <- DataModel$new()
  
  # Empty dataframe with correct structure
  empty_data <- data.frame(
    id = character(),
    site = character(),
    date = as.Date(character()),
    type = character(),
    value = numeric(),
    carbon_emission_kgco2e = numeric(),
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(empty_data)
  expect_true(isTRUE(result))
  
  # All values are NA but columns exist
  na_data <- data.frame(
    id = NA_character_,
    site = NA_character_,
    date = as.Date(NA),
    type = NA_character_,
    value = NA_real_,
    carbon_emission_kgco2e = NA_real_,
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(na_data)
  expect_true(is.list(result))
  expect_true(any(grepl("Unparseable dates", result$errors)))
})

test_that("validate reports multiple errors", {
  dm <- DataModel$new()
  
  # Data with multiple issues
  test_data <- data.frame(
    id = c("1", "2", "2"),  # Duplicate
    site = rep("SITE1", 3),
    date = as.Date(c("2024-01-01", NA, "2024-03-01")),  # NA date
    type = rep("Electric", 3),
    value = c(-100, 200, 300),  # Negative value
    carbon_emission_kgco2e = c(10, -20, 30),  # Negative emission
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  expect_true(length(result$errors) >= 4)  # At least 4 different errors
  expect_true(any(grepl("Unparseable dates", result$errors)))
  expect_true(any(grepl("Negative values in 'value'", result$errors)))
  expect_true(any(grepl("Negative values in 'carbon_emission_kgco2e'", result$errors)))
  expect_true(any(grepl("Duplicate 'id' values", result$errors)))
})

test_that("canonicalize handles whitespace properly", {
  dm <- DataModel$new()
  
  test_data <- data.frame(
    id = c("  123  ", "456\t\t", "789\n", "  101  112  "),
    site = c("  site1  ", "site2\t", "\nsite3", "  site  4  "),
    date = rep("2024-01-01", 4),
    type = c("  electric  ", "gas\t\t", "\nwater", "  solar  panel  "),
    value = c("  100  ", "200\t", "\n300", "  400  "),
    carbon_emission_kgco2e = rep("10", 4),
    stringsAsFactors = FALSE
  )
  
  result <- dm$canonicalize(test_data)
  
  # Check whitespace trimming and collapsing
  expect_equal(result$id[1], "123")
  expect_equal(result$id[2], "456")
  expect_equal(result$id[3], "789")
  expect_equal(result$id[4], "101 112")  # Internal spaces collapsed to single space
  
  expect_equal(result$site[1], "SITE1")
  expect_equal(result$site[4], "SITE 4")  # Internal space preserved but collapsed
  
  expect_equal(result$type[1], "Electric")
  expect_equal(result$type[4], "Solar Panel")
  
  expect_equal(result$value[1], 100)
  expect_equal(result$value[2], 200)
})

test_that("validate provides helpful error messages", {
  dm <- DataModel$new()
  
  # Test with specific row numbers in error messages
  test_data <- data.frame(
    id = as.character(1:10),
    site = rep("SITE1", 10),
    date = as.Date(rep("2024-01-01", 10)),
    type = rep("Electric", 10),
    value = c(100, -200, 300, -400, 500, -600, 700, 800, 900, 1000),
    carbon_emission_kgco2e = rep(10, 10),
    stringsAsFactors = FALSE
  )
  
  result <- dm$validate(test_data)
  
  expect_true(is.list(result))
  # Should mention specific row numbers (up to 5 examples)
  expect_true(any(grepl("rows 2, 4, 6", result$errors)))
  expect_true(any(grepl("3 row\\(s\\)", result$errors)))
})