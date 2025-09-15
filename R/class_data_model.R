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
                             
                             if ("id"   %in% names(out)) out$id   <- squish(out$id)
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
                           kpi_total_consumption = function() 0,
                           kpi_total_emissions = function() 0,
                           kpi_avg_daily_consumption = function() 0,
                           timeseries = function(by = c("day","month")) data.frame(),
                           compare = function(by = c("site","type")) data.frame(),
                           summary_table = function() data.frame()
                         ),
                         private = list(
                           parse_date_flex = function(x) {
                             x_chr <- as.character(x)
                             pats  <- c(
                               "%d-%m-%Y", "%d-%m-%y",  # 20-08-2025, 20-08-25
                               "%Y-%m-%d",              # 2025-08-20
                               "%m/%d/%Y", "%m/%d/%y",  # 08/20/2025, 08/20/25  (US)
                               "%d/%m/%Y", "%d/%m/%y"   # 20/08/2025, 20/08/25  (EU)
                             )
                             out <- as.Date(rep(NA_character_, length(x_chr)))
                             for (fmt in pats) {
                               idx <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)
                               if (!any(idx)) break
                               parsed <- suppressWarnings(readr::parse_date(x_chr[idx], format = fmt, locale = readr::locale()))
                               out[idx] <- as.Date(parsed)
                             }
                             out
                           }
                         )
                         
)