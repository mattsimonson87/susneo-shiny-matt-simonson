#' SUSNEO data model
#'
#' Core R6 class encapsulating canonicalization, validation, filtering,
#' KPI calculations, and the merge policy (Append + Override).
#'
#' @docType class
#' @format An [R6::R6Class] generator.
#'
#' @field dataset Current tibble of records.
#' @field sources Data frame of provenance rows (tag, time, appended, replaced, new_sites, new_types).
#' @field filters List with `date` (length-2 Date), `sites` (character), `types` (character).
#'
#' @section Methods:
#' \itemize{
#'   \item \code{initialize()}, \code{set_filters()}, \code{filtered_data()}
#'   \item \code{canonicalize()}, \code{validate()}, \code{merge()}
#'   \item \code{status()}, KPI methods, \code{timeseries()}, \code{compare()}, \code{summary_table()}
#' }
#' @export
DataModel <- R6::R6Class("DataModel",
                         public = list(
                           dataset = NULL,
                           sources = NULL,
                           filters = NULL,
                           #' @description Set active filters (date range, sites, types).
                           #' @param dates length-2 Date vector (min, max) or NULL.
                           #' @param sites character vector of site codes or NULL.
                           #' @param types character vector of types or NULL.
                           #' @return Invisibly returns `self`.
                           set_filters = function(dates = NULL, sites = NULL, types = NULL) {
                             if (!is.null(dates) && length(dates) == 2) {
                               self$filters$date <- dates
                             }
                             if (!is.null(sites)) {
                               self$filters$sites <- sites
                             }
                             if (!is.null(types)) {
                               self$filters$types <- types
                             }
                             invisible(self)
                           },
                           #' @description Return data after applying active filters.
                           #' @return A data.frame/tibble of filtered rows.
                           filtered_data = function() {
                             if (is.null(self$dataset)) {
                               return(data.frame())
                             }
                             
                             df <- self$dataset
                             
                             # Apply date filter
                             if (!is.null(self$filters$date) && length(self$filters$date) == 2) {
                               date_min <- self$filters$date[1]
                               date_max <- self$filters$date[2]
                               if (!is.na(date_min) && !is.na(date_max)) {
                                 df <- df[!is.na(df$date) & df$date >= date_min & df$date <= date_max, , drop = FALSE]
                               }
                             }
                             
                             # Apply site filter
                             if (!is.null(self$filters$sites) && length(self$filters$sites) > 0) {
                               df <- df[df$site %in% self$filters$sites, , drop = FALSE]
                             }
                             
                             # Apply type filter
                             if (!is.null(self$filters$types) && length(self$filters$types) > 0) {
                               df <- df[df$type %in% self$filters$types, , drop = FALSE]
                             }
                             
                             df
                           },
                           #' @description Initialize model and load bundled sample dataset.
                           #' @return A new `DataModel` object.
                           initialize = function() {
                             path <- system.file("extdata", "sample_data.csv", package = "susneoShinyMatt")
                             if (!nzchar(path) || !file.exists(path)) stop("Bundled sample not found at: ", path)
                             
                             df <- readr::read_csv(
                               path,
                               col_types = readr::cols(.default = readr::col_character()),
                               show_col_types = FALSE
                             )
                             
                             # map headers to standard names
                             names(df) <- canonical_names(names(df))
                             
                             req <- c("id","site","date","type","value","carbon_emission_kgco2e")
                             missing <- setdiff(req, names(df))
                             if (length(missing)) stop("Bundled sample is missing required columns: ", paste(missing, collapse = ", "))
                             
                             df <- self$canonicalize(df)
                             
                             self$dataset <- df
                             self$sources <- data.frame(
                               tag       = "sample",
                               time      = Sys.time(),
                               appended  = nrow(df),
                               replaced  = 0L,
                               new_sites = length(unique(df$site)),
                               new_types = length(unique(df$type)),
                               stringsAsFactors = FALSE
                             )
                             rng <- range(df$date, na.rm = TRUE)
                             self$filters <- list(
                               date  = rng,
                               sites = sort(unique(df$site)),
                               types = sort(unique(df$type))
                             )
                           },
                           #' @description Canonicalize input data (names, whitespace, case, types, dates).
                           #' @param df A data.frame-like object to canonicalize.
                           #' @return Canonicalized data.frame with required columns.
                           canonicalize = function(df) {
                             stopifnot(is.data.frame(df))
                             out <- df
                             names(out) <- canonical_names(names(out))
                             
                             # fast + NA-safe squish
                             squish_chr <- function(x) {
                               is_na <- is.na(x)
                               x_chr <- if (is.character(x)) x else as.character(x)
                               x_chr[!is_na] <- stringr::str_squish(x_chr[!is_na])
                               x_chr[is_na] <- NA_character_
                               x_chr
                             }
                             
                             # ID: trim/squish + strip trailing ".0" from numeric-like IDs
                             if ("id" %in% names(out)) {
                               out$id <- squish_chr(out$id)
                               out$id <- sub("^([0-9]+)\\.0+$", "\\1", out$id)
                             }
                             
                             # SITE: uppercase by unique, then map back (avoids N * string ops)
                             if ("site" %in% names(out)) {
                               s_raw <- out$site
                               u <- unique(s_raw)
                               u_out <- toupper(squish_chr(u))
                               map <- stats::setNames(u_out, as.character(u))
                               out$site <- unname(map[as.character(s_raw)])
                             }
                             
                             # TYPE: preserve ALL-CAPS; otherwise Title Case — by unique, then map back
                             if ("type" %in% names(out)) {
                               t_raw <- out$type
                               u <- unique(t_raw)
                               u_sq <- squish_chr(u)
                               keep_upper <- !is.na(u_sq) & (u_sq == toupper(u_sq))
                               u_out <- u_sq
                               u_out[!keep_upper] <- stringr::str_to_title(u_out[!keep_upper])
                               map <- stats::setNames(u_out, as.character(u))
                               out$type <- unname(map[as.character(t_raw)])
                             }
                             
                             # DATE: parse only if not already Date
                             if ("date" %in% names(out)) {
                               if (!inherits(out$date, "Date")) {
                                 out$date <- private$parse_date_flex(out$date)
                               }
                             }
                             
                             # NUMERICS: coerce quietly
                             for (nm in c("value", "carbon_emission_kgco2e")) {
                               if (nm %in% names(out)) out[[nm]] <- suppressWarnings(as.numeric(out[[nm]]))
                             }
                             
                             out
                           },
                           #' @description Validate a canonicalized dataset against the data contract.
                           #' @param df A data.frame to validate (usually after `canonicalize()`).
                           #' @return `TRUE` if valid; otherwise a list with element `errors` (character).
                           validate = function(df) {
                             req_cols <- c("id","site","date","type","value","carbon_emission_kgco2e")
                             errs <- character()
                             
                             # missing required columns
                             missing <- setdiff(req_cols, names(df))
                             if (length(missing)) {
                               errs <- c(errs, sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
                               return(list(errors = errs))  # stop early—other checks depend on these columns
                             }
                             
                             # unparseable dates (i.e., NA after canonicalize())
                             n_bad_date <- sum(is.na(df$date))
                             if (n_bad_date > 0) {
                               errs <- c(errs, sprintf("Unparseable dates: %d row(s) have NA in 'date' after parsing.", n_bad_date))
                             }
                             
                             # negative numeric values
                             for (nm in c("value","carbon_emission_kgco2e")) {
                               bad_idx <- which(!is.na(df[[nm]]) & df[[nm]] < 0)
                               if (length(bad_idx)) {
                                 errs <- c(errs, sprintf("Negative values in '%s': %d row(s) (e.g., rows %s).",
                                                         nm, length(bad_idx), paste(utils::head(bad_idx, 5), collapse = ", ")))
                               }
                             }
                             
                             # duplicate id within the upload (merge will keep last later; just flag here)
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
                           #' @description Merge an upload per policy (Append + Override; keep last within upload).
                           #' @param df Canonicalized data.frame to merge.
                           #' @param source_tag Optional tag/label for provenance.
                           #' @return Named list: `appended`, `replaced`, `new_sites`, `new_types` (integers).
                           merge = function(df, source_tag = NULL) {
                             stopifnot(is.data.frame(df))
                             
                             # Keep only the last occurrence per id 
                             df_last <- df[!duplicated(df$id, fromLast = TRUE), , drop = FALSE]
                             
                             # Work with the required contract columns to avoid rbind() column mismatches
                             req <- c("id","site","date","type","value","carbon_emission_kgco2e")
                             df_last <- df_last[, req, drop = FALSE]
                             
                             # Baseline (pre-merge) uniques for "new_*" counts
                             before_sites <- if (is.null(self$dataset)) character() else unique(self$dataset$site)
                             before_types <- if (is.null(self$dataset)) character() else unique(self$dataset$type)
                             
                             if (is.null(self$dataset)) {
                               existing <- self$dataset <- df_last
                               appended <- nrow(df_last)
                               replaced <- 0L
                             } else {
                               existing <- self$dataset[, req, drop = FALSE]
                               ids_upload   <- df_last$id
                               ids_existing <- existing$id
                               
                               # Remove any existing rows whose IDs appear in the upload (override)
                               keep_idx <- !(ids_existing %in% ids_upload)
                               existing_keep <- existing[keep_idx, , drop = FALSE]
                               
                               # Append the upload rows (now the only occurrence for those IDs)
                               self$dataset <- rbind(existing_keep, df_last)
                               
                               appended <- sum(!(ids_upload %in% ids_existing))           # new ids
                               replaced <- sum(ids_upload %in% ids_existing)              # overlapped ids (incl. identical content)
                             }
                             
                             # Update sources
                             # New sites/types introduced by this upload
                             new_sites <- setdiff(unique(df_last$site), before_sites)
                             new_types <- setdiff(unique(df_last$type), before_types)
                             
                             # Ensure sources is a data.frame (robust if older objects had a character vector)
                             if (is.null(self$sources)) {
                               self$sources <- data.frame(
                                 tag = character(), time = as.POSIXct(character()),
                                 appended = integer(), replaced = integer(), new_sites = integer(), new_types = integer(),
                                 stringsAsFactors = FALSE
                               )
                             } else if (!is.data.frame(self$sources)) {
                               self$sources <- data.frame(
                                 tag = as.character(self$sources), time = rep(Sys.time(), length(self$sources)),
                                 appended = NA_integer_, replaced = NA_integer_, new_sites = NA_integer_, new_types = NA_integer_,
                                 stringsAsFactors = FALSE
                               )
                             }
                             
                             # Tag for this upload
                             tag <- if (is.null(source_tag)) sprintf("upload_%s", format(Sys.time(), "%Y%m%d-%H%M%S")) else source_tag
                             
                             # Append provenance row
                             meta <- data.frame(
                               tag       = tag,
                               time      = Sys.time(),
                               appended  = as.integer(appended),
                               replaced  = as.integer(replaced),
                               new_sites = as.integer(length(new_sites)),
                               new_types = as.integer(length(new_types)),
                               stringsAsFactors = FALSE
                             )
                             self$sources <- rbind(self$sources, meta)
                             
                             # Refresh filters from merged data
                             rng <- range(self$dataset$date, na.rm = TRUE)
                             self$filters <- list(
                               date  = rng,
                               sites = sort(unique(self$dataset$site)),
                               types = sort(unique(self$dataset$type))
                             )
                             
                             list(
                               appended  = as.integer(appended),
                               replaced  = as.integer(replaced),
                               new_sites = as.integer(length(new_sites)),
                               new_types = as.integer(length(new_types))
                             )
                           },
                           #' @description Summary of current dataset, filters, and provenance.
                           #' @return List with counts, date range, sites/types, and last source/time.
                           status = function() {
                             if (is.null(self$dataset)) {
                               return(list(
                                 n_rows = 0, n_sites = 0, date_min = NA, date_max = NA,
                                 sites = character(), types = character(),
                                 sources_count = 0L, uploads_total = 0L,
                                 last_source = NA_character_, last_time = as.POSIXct(NA)
                               ))
                             }
                             
                             is_df <- is.data.frame(self$sources)
                             sc <- if (is.null(self$sources)) 0L else as.integer(NROW(self$sources))  # works for vector or df
                             
                             last_source <- if (sc > 0) {
                               if (is_df && "tag" %in% names(self$sources)) utils::tail(self$sources$tag, 1) else utils::tail(as.character(self$sources), 1)
                             } else NA_character_
                             
                             last_time <- if (is_df && sc > 0 && "time" %in% names(self$sources)) {
                               utils::tail(self$sources$time, 1)
                             } else {
                               as.POSIXct(NA)
                             }
                             
                             list(
                               n_rows = nrow(self$dataset),
                               n_sites = length(unique(self$dataset$site)),
                               date_min = min(self$dataset$date, na.rm = TRUE),
                               date_max = max(self$dataset$date, na.rm = TRUE),
                               sites = sort(unique(self$dataset$site)),
                               types = sort(unique(self$dataset$type)),
                               sources_count = sc,
                               uploads_total = max(0L, sc - 1L),  # first entry is the bundled sample
                               last_source = last_source,
                               last_time = last_time
                             )
                           },
                           #' @description Reset dataset and filters (keeps sources unless clear_sources=TRUE).
                           #' @param clear_sources logical; if TRUE also clears provenance history.
                           #' @return Invisibly returns `self`.
                           reset = function(clear_sources = FALSE) {
                             self$dataset <- NULL
                             self$filters <- list(date = NULL, sites = NULL, types = NULL)
                             if (isTRUE(clear_sources)) self$sources <- NULL
                             invisible(self)
                           },
                           #' @description Total consumption under current filters.
                           #' @return Numeric scalar.
                           kpi_total_consumption = function() {
                             df <- self$filtered_data()
                             sum(df$value, na.rm = TRUE)
                           },
                           #' @description Total emissions under current filters.
                           #' @return Numeric scalar.
                           kpi_total_emissions = function() {
                             df <- self$filtered_data()
                             sum(df$carbon_emission_kgco2e, na.rm = TRUE)
                           },
                           #' @description Average daily consumption over the filtered date range.
                           #' @return Numeric scalar.
                           kpi_avg_daily_consumption = function() {
                             df <- self$filtered_data()
                             if (!nrow(df)) return(0)
                             rng <- range(df$date, na.rm = TRUE)
                             days <- as.integer(rng[2] - rng[1]) + 1L
                             if (is.na(days) || days <= 0) return(0)
                             sum(df$value, na.rm = TRUE) / days
                           },
                           #' @description Energy intensity = total consumption / distinct site-days.
                           #' @return Numeric scalar.
                           kpi_energy_intensity = function() {
                             df <- self$filtered_data()
                             if (!nrow(df)) return(0)
                             sd <- nrow(dplyr::distinct(df, site, date))  # site-day combos
                             if (sd <= 0) return(0)
                             sum(df$value, na.rm = TRUE) / sd
                           },
                           #' @description Aggregated time series for plots.
                           #' @param by One of `"day"` or `"month"`.
                           #' @return Data.frame with columns `period`, `value`, `emissions`.
                           timeseries = function(by = c("day","month")) {
                             by <- match.arg(by)
                             df <- self$filtered_data()
                             if (!nrow(df)) return(df[0, , drop = FALSE])
                             df$period <- if (by == "month") as.Date(cut(df$date, "month")) else df$date
                             dplyr::summarise(
                               dplyr::group_by(df, period),
                               value    = sum(value, na.rm = TRUE),
                               emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
                               .groups = "drop"
                             ) |>
                               dplyr::arrange(period)
                           },
                           #' @description Aggregations for comparison chart.
                           #' @param by One of `"site"` or `"type"`.
                           #' @return Data.frame grouped by `by` with totals.
                           compare = function(by = c("site","type")) {
                             by <- match.arg(by)
                             df <- self$filtered_data()
                             if (!nrow(df)) return(df[0, , drop = FALSE])
                             out <- dplyr::summarise(
                               dplyr::group_by(df, dplyr::across(dplyr::all_of(by))),
                               value    = sum(value, na.rm = TRUE),
                               emissions = sum(carbon_emission_kgco2e, na.rm = TRUE),
                               .groups = "drop"
                             )
                             out[order(out$value, decreasing = TRUE), , drop = FALSE]
                           },
                           #' @description Summary table backing the dashboard (respects filters).
                           #' @return Data.frame of currently filtered rows.
                           summary_table = function() {
                             self$filtered_data()
                           }
                         ),
                         private = list(
                           parse_date_flex = function(x) {
                             x_chr <- as.character(x)
                             out   <- as.Date(rep(NA_character_, length(x_chr)))
                             
                             # Excel serials first
                             num     <- suppressWarnings(as.numeric(x_chr))
                             idx_num <- !is.na(num) & grepl("^\\s*\\d+(\\.0+)?\\s*$", x_chr)
                             if (any(idx_num)) {
                               idx_pos <- which(idx_num)
                               
                               # Excel 1900 date system 
                               d <- as.Date(num[idx_pos], origin = "1899-12-30")
                               plausible <- d >= as.Date("1900-01-01") & d <= as.Date("2100-12-31")
                               out[idx_pos[plausible]] <- d[plausible]
                               
                               # Fallback: Excel 1904 system 
                               bad_within <- which(!plausible)
                               if (length(bad_within)) {
                                 i_bad <- idx_pos[bad_within]
                                 d2 <- as.Date(num[i_bad], origin = "1904-01-01")
                                 plausible2 <- d2 >= as.Date("1904-01-01") & d2 <= as.Date("2100-12-31")
                                 out[i_bad[plausible2]] <- d2[plausible2]
                               }
                             }
                             
                             # Try common textual formats (DMY first to match "20-08-2025")
                             pats <- c(
                               "%d-%m-%Y", "%d-%m-%y",
                               "%Y-%m-%d",
                               "%m/%d/%Y", "%m/%d/%y",
                               "%d/%m/%Y", "%d/%m/%y",
                               "%d-%b-%Y", "%b %d, %Y", "%d %b %Y",
                               "%Y/%m/%d"
                             )
                             for (fmt in pats) {
                               idx <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)
                               if (!any(idx)) break
                               parsed <- suppressWarnings(readr::parse_date(x_chr[idx], format = fmt, locale = readr::locale()))
                               out[idx] <- as.Date(parsed)
                             }
                             
                             # Last resort: parse datetime, then drop time
                             idx <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)
                             if (any(idx)) {
                               parsed_dt <- suppressWarnings(readr::parse_datetime(x_chr[idx], locale = readr::locale()))
                               out[idx] <- as.Date(parsed_dt)
                             }
                             
                             out
                           }
                           
                         )
                         
)