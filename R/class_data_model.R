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
                             self$sources <- c("sample")
                             rng <- range(df$date, na.rm = TRUE)
                             self$filters <- list(
                               date  = rng,
                               sites = sort(unique(df$site)),
                               types = sort(unique(df$type))
                             )

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
                             names(out) <- canonical_names(names(out))
                             # helper: trim + collapse internal spaces
                             squish <- function(x) gsub("\\s+", " ", trimws(as.character(x)))
                             
                             if ("id" %in% names(out)) {
                               out$id <- squish(out$id)
                               out$id <- sub("^([0-9]+)\\.0+$", "\\1", out$id)
                             }
                             if ("site" %in% names(out)) out$site <- toupper(squish(out$site))             # UPPERCASE
                             if ("type" %in% names(out)) out$type <- tools::toTitleCase(squish(out$type))  # Title Case
                             
                             if ("date" %in% names(out)) {
                               if (!inherits(out$date, "Date")) {
                                 out$date <- private$parse_date_flex(out$date)
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
                           merge = function(df, source_tag = NULL) {
                             stopifnot(is.data.frame(df))
                             
                             # Keep only the last occurrence per id (policy: "last wins")
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
                             tag <- if (is.null(source_tag)) sprintf("upload_%s", format(Sys.time(), "%Y%m%d-%H%M%S")) else source_tag
                             self$sources <- c(self$sources, tag)
                             
                             # Refresh filters from merged data
                             rng <- range(self$dataset$date, na.rm = TRUE)
                             self$filters <- list(
                               date  = rng,
                               sites = sort(unique(self$dataset$site)),
                               types = sort(unique(self$dataset$type))
                             )
                             
                             # New sites/types introduced by this upload
                             new_sites <- setdiff(unique(df_last$site), before_sites)
                             new_types <- setdiff(unique(df_last$type), before_types)
                             
                             list(
                               appended  = as.integer(appended),
                               replaced  = as.integer(replaced),
                               new_sites = as.integer(length(new_sites)),
                               new_types = as.integer(length(new_types))
                             )
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
                           kpi_total_consumption = function() {
                             df <- self$filtered_data()
                             sum(df$value, na.rm = TRUE)
                           },
                           
                           kpi_total_emissions = function() {
                             df <- self$filtered_data()
                             sum(df$carbon_emission_kgco2e, na.rm = TRUE)
                           },
                           
                           kpi_avg_daily_consumption = function() {
                             df <- self$filtered_data()
                             if (!nrow(df)) return(0)
                             rng <- range(df$date, na.rm = TRUE)
                             days <- as.integer(rng[2] - rng[1]) + 1L
                             if (is.na(days) || days <= 0) return(0)
                             sum(df$value, na.rm = TRUE) / days
                           },
                           
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