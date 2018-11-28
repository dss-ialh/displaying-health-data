rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
#Load any source files that contain/define functions, but that don't load any other types of variables
#   into memory.  Avoid side effects and don't pollute the global environment.
# source("./SomethingSomething.R")
source("./scripts/common-functions.R") # used in multiple reports
source("./scripts/graphing/graph-presets.R") # fonts, colors, themes
source("./scripts/graphing/graph-alluvia.R")

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
requireNamespace("dplyr")
# requireNamespace("RColorBrewer")
# requireNamespace("scales") #For formating values in graphs
# requireNamespace("mgcv) #For the Generalized Additive Model that smooths the longitudinal graphs.
# requireNamespace("TabularManifest") # devtools::install_github("Melinae/TabularManifest")
library(ggalluvial)
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
    ,id_counter = 1
  ) %>%
  dplyr::mutate(
    gender_id = factor(gender_id, levels = c(1,2, 255), labels = c("Men","Women", "Missing"))
  )

# ---- basic-graph -------------------------------------------------------------
ds %>% dplyr::glimpse()

axis1_ = "gender_id"
axis2_ = "race"
axis1_label = axis1_
axis1_label = toupper(axis1_)


d_input <- ds %>%
  # rename the target variable for easier syntax (part 1)
  dplyr::group_by(gender_id, age_cut_4, race) %>%
  dplyr::summarize(
    target_metric = length(unique(id_counter))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(gender_id, age_cut_4)
# rename it back (part 2)
d_input

g1 <- d_input %>%
  ggplot2::ggplot(
    aes_string(
      # y     = "target_metric"
      # ,axis1 = "gender_id"
      # ,axis2 = "age_cut_4"
      # ,fill = "race"
      y     = "target_metric"
      ,axis1 = axis1_#"gender_id"
      ,axis2 = axis2_ #"race"
      ,fill = "age_cut_4"
    )
  )
g1
g2 <- g1 +
    ggalluvial::geom_alluvium(
      # fill = "firebrick",
      alpha = .5
    )+
  ggalluvial::geom_stratum(alpha = 0, color = "grey40")+
  # ggplot2::geom_text(stat = "stratum", label.strata = TRUE)
  ggrepel::geom_text_repel( # helps with legends to be more readable
    stat = "stratum", label.strata = TRUE, direction = "both", color = "black", size = baseSize -6
  )+
  scale_x_discrete(
    # limits = c("gender_id", "race")
    limits = c(axis1_, axis2_)
    , expand = c(.05, .05)
    ,position = "top"
  )
g2
# ---- scatterplots ------------------------------------------------------------

# ---- models ------------------------------------------------------------------

# ---- model-results-table  -----------------------------------------------
