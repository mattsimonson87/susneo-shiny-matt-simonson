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
                           canonicalize = function(df) df,
                           validate = function(df) TRUE,
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