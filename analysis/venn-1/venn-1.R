rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")
source("./scripts/functions-common.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes
source("./scripts/graphing/graph-venn-diagram.R")


# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
requireNamespace("dplyr")
# requireNamespace("RColorBrewer")
# requireNamespace("scales") #For formating values in graphs
# requireNamespace("mgcv) #For the Generalized Additive Model that smooths the longitudinal graphs.
# requireNamespace("TabularManifest") # devtools::install_github("Melinae/TabularManifest")

# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values
config                      <- config::get()
# Uncomment the lines above and delete the one below if value is stored in 'config.yml'.

(path_input <- config$path_mlm_1_derived)

# ---- load-data ---------------------------------------------------------------
ds <- readr::read_rds(path_input) # 'ds' stands for 'datasets'
ds %>% dplyr::glimpse(50)

# ---- tweak-data --------------------------------------------------------------
ds <- ds %>%
  dplyr::rename(
    age_at_visit = age
    # ,date_at_visit = year
    ,fu_year = wave_id
  )

# ---- basic-graph -------------------------------------------------------------

# ---- scatterplots ------------------------------------------------------------

# ---- models ------------------------------------------------------------------

# ---- model-results-table  -----------------------------------------------
