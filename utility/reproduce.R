# knitr::stitch_rmd(script="./manipulation/osdh/osdh-flow.R", output="./stitched-output/manipulation/osdh/osdh-flow.md")
#This next line is run when the whole file is executed, but not when knitr calls individual chunks.
rm(list=ls(all=TRUE)) #Clear the memory for any variables set from any previous runs.

# ---- load-sources ------------------------------------------------------------


# ---- load-packages -----------------------------------------------------------
library(magrittr)
requireNamespace("testit")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()
path_db                        <- config$path_database

ds_rail  <- tibble::tribble(
  ~fx             , ~path,

  # Simulate observed data
  "run_file_r"    , "manipulation/simulation/simulate-mlm-1.R",

  # Ellis Lanes
  "run_file_r"    , "manipulation/mlm-1-ellis.R",
  "run_file_r"    , "manipulation/subject-1-ellis.R",
  "run_file_r"    , "manipulation/te-ellis.R",

  # Scribes
  "run_file_r"    , "manipulation/mlm-1-scribe.R",
  "run_file_r"    , "manipulation/te-scribe.R",

  # Reports

  "run_rmd"       , "analysis/report-te-1/report-te-1.Rmd",

  # Dashboards
  "run_rmd"       , "analysis/dashboard-1/dashboard-1.Rmd"
)

run_file_r <- function( minion ) {
  message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
  base::source(minion, local=new.env())
  message("Completed `", basename(minion), "`.")
  return( TRUE )
}
# run_ferry_sql <- function( minion ) {
#   message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
#   OuhscMunge::execute_sql_file(minion, config_value("dsn_miechv"))
#   message("Completed `", basename(minion), "`.")
#   return( TRUE )
# }
run_rmd <- function( minion ) {
  message("\nStarting `", basename(minion), "` at ", Sys.time(), ".")
  path_out <- rmarkdown::render(minion, envir=new.env())
  Sys.sleep(3) # Sleep for three secs, to let pandoc finish
  message(path_out)
  return( TRUE )
}

(file_found <- purrr::map_lgl(ds_rail$path, file.exists))
if( !all(file_found) ) {
  warning("--Missing files-- \n", paste0(ds_rail$path[!file_found], collapse="\n"))
  stop("All source files to be run should exist.")
}

# ---- load-data ---------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------

# ---- run-sources -------------------------------------------------------------

message("Preparing to run\n\t", paste(ds_rail$path, collapse="\n\t"))

warn_level_initial <- as.integer(options("warn"))
# options(warn=0)  # warnings are stored until the top–level function returns
# options(warn=2)  # treat warnings as errors

(start_time <- Sys.time())

# Remove old DB
# if( file.exists(path_db) ) file.remove(path_db)

purrr::invoke_map_lgl(
  ds_rail$fx,
  ds_rail$path
)

(elapsed_duration <-  difftime(Sys.time(), start_time, units="min"))
options(warn=warn_level_initial)  # Restore the whatever warning level you started with.

message("Completed osdh-flow at ", Sys.time(), " (in ", round(elapsed_duration, 2), " mins.)")

# ---- verify-values -----------------------------------------------------------
