#' DataModel: ingestion, validation, merge, filters, KPIs
#' @noRd
DataModel <- R6::R6Class("DataModel",
                         public = list(
                           dataset = NULL,
                           sources = NULL,
                           filters = NULL,
                           initialize = function() {
                             path <- system.file("extdata", "sample_data.csv", package = "susneoShinyMatt")
                             if (!nzchar(path) || !file.exists(path)) stop("Bundled sample not found at: ", path)
                             
                             df <- readr::read_csv(path, show_col_types = FALSE)
                             
                             # map headers to standard names
                             names(df) <- canonical_names(names(df))
                             
                             req <- c("id","site","date","type","value","carbon_emission_kgco2e")
                             missing <- setdiff(req, names(df))
                             if (length(missing)) stop("Bundled sample is missing required columns: ", paste(missing, collapse = ", "))
                             
                             df$date <- as.Date(df$date)
                             
                             self$dataset <- df
                             self$sources <- c("sample")
                             rng <- range(df$date, na.rm = TRUE)
                             self$filters <- list(
                               date  = rng,
                               sites = sort(unique(df$site)),
                               types = sort(unique(df$type))
                             )
                           },
                           canonicalize = function(df) {
                             stopifnot(is.data.frame(df))
                             out <- df
                             
                             # helper: trim + collapse internal spaces
                             squish <- function(x) gsub("\\s+", " ", trimws(as.character(x)))
                             
                             if ("id"   %in% names(out)) out$id   <- squish(out$id)
                             if ("site" %in% names(out)) out$site <- toupper(squish(out$site))             # UPPERCASE
                             if ("type" %in% names(out)) out$type <- tools::toTitleCase(squish(out$type))  # Title Case
                             
                             if ("date" %in% names(out)) {
                               # If not already Date, try ISO-8601 "YYYY-MM-DD". Unparseable -> NA (caught in validate()).
                               if (!inherits(out$date, "Date")) {
                                 out$date <- as.Date(as.character(out$date))
                               }
                             }
                             
                             for (nm in c("value", "carbon_emission_kgco2e")) {
                               if (nm %in% names(out)) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
                             }
                             
                             out
                           },
                           validate = function(df) {
                             req_cols <- c("id","site","date","type","value","carbon_emission_kgco2e")
                             errs <- character()
                             
                             # 1) missing required columns
                             missing <- setdiff(req_cols, names(df))
                             if (length(missing)) {
                               errs <- c(errs, sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
                               return(list(errors = errs))  # stop early—other checks depend on these columns
                             }
                             
                             # 2) unparseable dates (i.e., NA after canonicalize())
                             n_bad_date <- sum(is.na(df$date))
                             if (n_bad_date > 0) {
                               errs <- c(errs, sprintf("Unparseable dates: %d row(s) have NA in 'date' after parsing.", n_bad_date))
                             }
                             
                             # 3) negative numeric values
                             for (nm in c("value","carbon_emission_kgco2e")) {
                               bad_idx <- which(!is.na(df[[nm]]) & df[[nm]] < 0)
                               if (length(bad_idx)) {
                                 errs <- c(errs, sprintf("Negative values in '%s': %d row(s) (e.g., rows %s).",
                                                         nm, length(bad_idx), paste(utils::head(bad_idx, 5), collapse = ", ")))
                               }
                             }
                             
                             # 4) duplicate id within the upload (merge will keep last later; just flag here)
                             dup_tbl <- dplyr::count(df, id, name = "n")
                             dup_tbl <- dplyr::filter(dup_tbl, n > 1)
                             if (nrow(dup_tbl) > 0) {
                               example_ids <- paste(utils::head(dup_tbl$id, 5), collapse = ", ")
                               total_extra <- sum(dup_tbl$n) - nrow(dup_tbl)  # how many extras beyond 1 each
                               errs <- c(errs, sprintf("Duplicate 'id' values: %d unique id(s) duplicated, %d extra row(s). Examples: %s. (Merge will keep last occurrence.)",
                                                       nrow(dup_tbl), total_extra, example_ids))
                             }
                             
                             if (length(errs) == 0) TRUE else list(errors = errs)
                           },
                           merge = function(df, source_tag = "upload_1") {
                             list(appended = 0, replaced = 0, new_sites = 0, new_types = 0)
                           },
                           reset = function() list(n_rows = 0, n_sites = 0),
                           set_filters = function(date_range, sites = NULL, types = NULL) {
                             self$filters$date  <- date_range
                             self$filters$sites <- sites
                             self$filters$types <- types
                             invisible(self)
                           },
                           filtered_data = function() {
                             df <- self$dataset
                             f  <- self$filters
                             if (!is.null(f$date))  df <- dplyr::filter(df, date >= f$date[1], date <= f$date[2])
                             if (!is.null(f$sites) && length(f$sites)) df <- dplyr::filter(df, site %in% f$sites)
                             if (!is.null(f$types) && length(f$types)) df <- dplyr::filter(df, type %in% f$types)
                             df
                           },
                           status = function() {
                             if (is.null(self$dataset)) {
                               return(list(n_rows = 0, n_sites = 0, date_min = NA, date_max = NA,
                                           sites = character(), types = character(), sources_count = length(self$sources)))
                             }
                             list(
                               n_rows = nrow(self$dataset),
                               n_sites = length(unique(self$dataset$site)),
                               date_min = min(self$dataset$date, na.rm = TRUE),
                               date_max = max(self$dataset$date, na.rm = TRUE),
                               sites = sort(unique(self$dataset$site)),
                               types = sort(unique(self$dataset$type)),
                               sources_count = length(self$sources)
                             )
                           },
                           kpi_total_consumption = function() 0,
                           kpi_total_emissions = function() 0,
                           kpi_avg_daily_consumption = function() 0,
                           timeseries = function(by = c("day","month")) data.frame(),
                           compare = function(by = c("site","type")) data.frame(),
                           summary_table = function() data.frame()
                         )
)