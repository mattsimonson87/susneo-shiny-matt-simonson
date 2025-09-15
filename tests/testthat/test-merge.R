# test-merge.R
# Tests for DataModel merge functionality

# Helper function to create a test DataModel with controlled data
create_test_dm_for_merge <- function(test_data) {
  dm <- DataModel$new()
  
  # Replace dataset and update filters/sources to match
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
  
  # Reset sources to clean state for testing
  dm$sources <- data.frame(
    tag = character(),
    time = as.POSIXct(character()),
    appended = integer(),
    replaced = integer(),
    new_sites = integer(),
    new_types = integer(),
    stringsAsFactors = FALSE
  )
  
  return(dm)
}

test_that("merge adds new records correctly", {
  # Create DM with controlled initial data
  initial_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # New data to merge
  new_data <- data.frame(
    id = c("2", "3"),
    site = c("SITE2", "SITE3"),
    date = as.Date(c("2024-02-01", "2024-03-01")),
    type = c("Gas", "Water"),
    value = c(200, 300),
    carbon_emission_kgco2e = c(20, 30),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(new_data, source_tag = "test_upload")
  
  expect_equal(result$appended, 2)
  expect_equal(result$replaced, 0)
  expect_equal(nrow(dm$dataset), 3)
  expect_true("2" %in% dm$dataset$id)
  expect_true("3" %in% dm$dataset$id)
})

test_that("merge replaces existing records with same ID", {
  # Set initial data
  initial_data <- data.frame(
    id = c("1", "2", "3"),
    site = c("SITE1", "SITE2", "SITE3"),
    date = as.Date(c("2024-01-01", "2024-02-01", "2024-03-01")),
    type = c("Electric", "Gas", "Water"),
    value = c(100, 200, 300),
    carbon_emission_kgco2e = c(10, 20, 30),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # Data with overlapping IDs
  update_data <- data.frame(
    id = c("2", "4"),
    site = c("SITE2_UPDATED", "SITE4"),
    date = as.Date(c("2024-02-15", "2024-04-01")),
    type = c("Gas", "Electric"),
    value = c(250, 400),
    carbon_emission_kgco2e = c(25, 40),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(update_data, source_tag = "test_update")
  
  expect_equal(result$appended, 1)  # ID "4" is new
  expect_equal(result$replaced, 1)  # ID "2" was replaced
  expect_equal(nrow(dm$dataset), 4)
  
  # Check that ID "2" was updated
  row2 <- dm$dataset[dm$dataset$id == "2", ]
  expect_equal(row2$site, "SITE2_UPDATED")
  expect_equal(row2$value, 250)
})

test_that("merge handles duplicate IDs in upload (keeps last)", {
  initial_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # Upload with duplicate IDs
  dup_data <- data.frame(
    id = c("2", "2", "2", "3"),
    site = c("SITE2_V1", "SITE2_V2", "SITE2_V3", "SITE3"),
    date = as.Date(c("2024-02-01", "2024-02-02", "2024-02-03", "2024-03-01")),
    type = c("Gas", "Gas", "Gas", "Water"),
    value = c(200, 210, 220, 300),
    carbon_emission_kgco2e = c(20, 21, 22, 30),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(dup_data, source_tag = "test_duplicates")
  
  expect_equal(result$appended, 2)  # Only 2 unique new IDs
  expect_equal(nrow(dm$dataset), 3)
  
  # Check that last occurrence of ID "2" was kept
  row2 <- dm$dataset[dm$dataset$id == "2", ]
  expect_equal(row2$site, "SITE2_V3")
  expect_equal(row2$value, 220)
})

test_that("merge tracks new sites and types correctly", {
  initial_data <- data.frame(
    id = c("1", "2"),
    site = c("SITE1", "SITE1"),
    date = as.Date(c("2024-01-01", "2024-01-02")),
    type = c("Electric", "Electric"),
    value = c(100, 150),
    carbon_emission_kgco2e = c(10, 15),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # Data with new sites and types
  new_data <- data.frame(
    id = c("3", "4", "5"),
    site = c("SITE2", "SITE3", "SITE1"),
    date = as.Date(c("2024-02-01", "2024-02-02", "2024-02-03")),
    type = c("Gas", "Water", "Electric"),
    value = c(200, 300, 400),
    carbon_emission_kgco2e = c(20, 30, 40),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(new_data, source_tag = "test_new_dimensions")
  
  expect_equal(result$new_sites, 2)  # SITE2 and SITE3 are new
  expect_equal(result$new_types, 2)  # Gas and Water are new
})

test_that("merge updates filters after merging", {
  initial_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-06-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  initial_filters <- dm$filters
  
  # Data with wider date range and new sites/types
  new_data <- data.frame(
    id = c("2", "3"),
    site = c("SITE2", "SITE3"),
    date = as.Date(c("2024-01-01", "2024-12-31")),
    type = c("Gas", "Water"),
    value = c(200, 300),
    carbon_emission_kgco2e = c(20, 30),
    stringsAsFactors = FALSE
  )
  
  dm$merge(new_data, source_tag = "test_filters")
  
  # Check date range expanded
  expect_equal(dm$filters$date[1], as.Date("2024-01-01"))
  expect_equal(dm$filters$date[2], as.Date("2024-12-31"))
  
  # Check sites include new ones
  expect_true("SITE2" %in% dm$filters$sites)
  expect_true("SITE3" %in% dm$filters$sites)
  
  # Check types include new ones
  expect_true("Gas" %in% dm$filters$types)
  expect_true("Water" %in% dm$filters$types)
})

test_that("merge updates sources tracking correctly", {
  initial_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  new_data <- data.frame(
    id = "2",
    site = "SITE2",
    date = as.Date("2024-02-01"),
    type = "Gas",
    value = 200,
    carbon_emission_kgco2e = 20,
    stringsAsFactors = FALSE
  )
  
  # First merge
  dm$merge(new_data, source_tag = "upload_1")
  expect_equal(nrow(dm$sources), 1)
  expect_equal(dm$sources$tag[1], "upload_1")
  expect_equal(dm$sources$appended[1], 1)
  
  # Second merge
  more_data <- data.frame(
    id = c("3", "2"),  # One new, one replacement
    site = c("SITE3", "SITE2"),
    date = as.Date(c("2024-03-01", "2024-02-15")),
    type = c("Water", "Gas"),
    value = c(300, 250),
    carbon_emission_kgco2e = c(30, 25),
    stringsAsFactors = FALSE
  )
  
  dm$merge(more_data, source_tag = "upload_2")
  expect_equal(nrow(dm$sources), 2)
  expect_equal(dm$sources$tag[2], "upload_2")
  expect_equal(dm$sources$appended[2], 1)
  expect_equal(dm$sources$replaced[2], 1)
})

test_that("merge handles NULL dataset (first merge)", {
  dm <- DataModel$new()
  dm$dataset <- NULL
  dm$filters <- NULL
  dm$sources <- NULL
  
  first_data <- data.frame(
    id = c("1", "2"),
    site = c("SITE1", "SITE2"),
    date = as.Date(c("2024-01-01", "2024-02-01")),
    type = c("Electric", "Gas"),
    value = c(100, 200),
    carbon_emission_kgco2e = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(first_data, source_tag = "initial_load")
  
  expect_equal(result$appended, 2)
  expect_equal(result$replaced, 0)
  expect_equal(nrow(dm$dataset), 2)
  expect_false(is.null(dm$dataset))
})

test_that("merge handles edge cases correctly", {
  initial_data <- data.frame(
    id = c("1", "2"),
    site = c("SITE1", "SITE2"),
    date = as.Date(c("2024-01-01", "2024-02-01")),
    type = c("Electric", "Gas"),
    value = c(100, 200),
    carbon_emission_kgco2e = c(10, 20),
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # Empty merge
  empty_data <- data.frame(
    id = character(),
    site = character(),
    date = as.Date(character()),
    type = character(),
    value = numeric(),
    carbon_emission_kgco2e = numeric(),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(empty_data, source_tag = "empty_merge")
  expect_equal(result$appended, 0)
  expect_equal(result$replaced, 0)
  expect_equal(nrow(dm$dataset), 2)
  
  # All replacements, no new
  replace_all <- data.frame(
    id = c("1", "2"),
    site = c("SITE1_NEW", "SITE2_NEW"),
    date = as.Date(c("2024-01-15", "2024-02-15")),
    type = c("Electric", "Gas"),
    value = c(150, 250),
    carbon_emission_kgco2e = c(15, 25),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(replace_all, source_tag = "replace_all")
  expect_equal(result$appended, 0)
  expect_equal(result$replaced, 2)
  expect_equal(nrow(dm$dataset), 2)
})

test_that("merge preserves column order and required columns", {
  initial_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # Data with extra columns (should be ignored)
  new_data <- data.frame(
    id = "2",
    extra_col = "should_be_ignored",
    site = "SITE2",
    date = as.Date("2024-02-01"),
    type = "Gas",
    value = 200,
    carbon_emission_kgco2e = 20,
    another_extra = 999,
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(new_data, source_tag = "test_columns")
  
  # Check that only required columns are present
  required_cols <- c("id", "site", "date", "type", "value", "carbon_emission_kgco2e")
  expect_equal(names(dm$dataset), required_cols)
  expect_equal(nrow(dm$dataset), 2)
})

test_that("merge handles NA values in data", {
  initial_data <- data.frame(
    id = "1",
    site = "SITE1",
    date = as.Date("2024-01-01"),
    type = "Electric",
    value = 100,
    carbon_emission_kgco2e = 10,
    stringsAsFactors = FALSE
  )
  
  dm <- create_test_dm_for_merge(initial_data)
  
  # Data with some NA values
  na_data <- data.frame(
    id = c("2", "3"),
    site = c("SITE2", "SITE3"),
    date = as.Date(c("2024-02-01", NA)),
    type = c("Gas", "Water"),
    value = c(NA, 300),
    carbon_emission_kgco2e = c(20, NA),
    stringsAsFactors = FALSE
  )
  
  result <- dm$merge(na_data, source_tag = "test_na")
  
  expect_equal(result$appended, 2)
  expect_equal(nrow(dm$dataset), 3)
  
  # Check NA values are preserved
  row2 <- dm$dataset[dm$dataset$id == "2", ]
  expect_true(is.na(row2$value))
  
  row3 <- dm$dataset[dm$dataset$id == "3", ]
  expect_true(is.na(row3$date))
  expect_true(is.na(row3$carbon_emission_kgco2e))
})