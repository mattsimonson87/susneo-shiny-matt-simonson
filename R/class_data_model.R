#' DataModel: ingestion, validation, merge, filters, KPIs
#' @noRd
DataModel <- R6::R6Class("DataModel",
                         public = list(
                           dataset = NULL,
                           sources = NULL,
                           initialize = function() { },
                           canonicalize = function(df) df,
                           validate = function(df) TRUE,
                           merge = function(df, source_tag = "upload_1") {
                             list(appended = 0, replaced = 0, new_sites = 0, new_types = 0)
                           },
                           reset = function() list(n_rows = 0, n_sites = 0),
                           set_filters = function(date_range, sites = NULL, types = NULL) invisible(self),
                           filtered_data = function() self$dataset,
                           status = function() list(n_rows = 0, n_sites = 0, date_min = NA, date_max = NA, sources_count = 1),
                           kpi_total_consumption = function() 0,
                           kpi_total_emissions = function() 0,
                           kpi_avg_daily_consumption = function() 0,
                           timeseries = function(by = c("day","month")) data.frame(),
                           compare = function(by = c("site","type")) data.frame(),
                           summary_table = function() data.frame()
                         )
)