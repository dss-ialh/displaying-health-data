rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.


# ---- load-sources ------------------------------------------------------------
base::source(file="./analysis/common/display-1.R") #Load common graphing functions.

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2) #For graphing
library(plotly)
requireNamespace("scales")
requireNamespace("dplyr")
requireNamespace("tidyr") #For converting wide to long
requireNamespace("broom")
requireNamespace("kableExtra")
requireNamespace("TabularManifest") # devtools::install_github("Melinae/TabularManifest"

# ---- declare-globals ---------------------------------------------------------
options(show.signif.stars=F) #Turn off the annotations on p-values
config                         <- config::get()

# desired_models            <- "PAT"
county_focus              <- 55L
base_size                 <- 14L

# path_in_annotation      <- "./data-public/raw/programs/cqi-annotation-example.csv"
# colors <- c('#0000ff','#ff8000','#ffff99',   '#ff0000' )
# palette_change_light <- list("increase"="#94c2cf", "no_change"="#dddddd", "decrease"="#b4837566")
# palette_change_dark  <- list("increase"="#4c6c83", "no_change"="#444444", "decrease"="#7d3b3d66")

# ---- load-data ---------------------------------------------------------------
ds                <- readr::read_rds(config$path_mlm_1_derived)
ds_county         <- readr::read_rds(config$path_county_derived)
ds_county_year    <- readr::read_rds(config$path_county_year_derived)

# ds_annotation       <- read.csv(config$path_annotation)

# ---- tweak-data --------------------------------------------------------------

ds <-
  ds %>%
  # dplyr::filter(county %in% desired_counties) %>%
  dplyr::mutate(
    emphasis        = dplyr::if_else(county_id == county_focus, "focus", "background"),
    county_id       = factor(county_id)
  )

# ---- headline-graph ----------------------------------------------------------
# cat("\n\n\n### Goals Status-- (ALL REPORTING PERIOD)\n\n\n")


# names(ds_client_week)


# ---- tables-county-year ----------------------------------------------------------
ds_county_year %>%
   dplyr::arrange(desc(year), county) %>%
   dplyr::select(
     county, year, cog_1_mean, cog_2_mean, cog_3_mean, phys_1_mean, phys_2_mean, phys_3_mean
   )%>%
   DT::datatable(
     colnames=gsub("_", " ", colnames(.)),
     options = list(
       pageLength = 16
     )
   ) %>%
  DT::formatCurrency(
    columns  = c(
      "cog_1_mean", "cog_2_mean", "cog_3_mean",
      "phys_1_mean", "phys_2_mean", "phys_3_mean"
    ),
    currency = "",
    digits   = 1
  )



# ---- tables-county ----------------------------------------------------------
ds_county  %>%
  dplyr::arrange(county) %>%
  dplyr::select(
    county, cog_1_mean, cog_2_mean, cog_3_mean, phys_1_mean, phys_2_mean, phys_3_mean
  )%>%
  DT::datatable(
    colnames=gsub("_", " ", colnames(.)),
    options = list(
      pageLength = 16
    )
  ) %>%
  DT::formatCurrency(
    columns  = c(
      "cog_1_mean", "cog_2_mean", "cog_3_mean",
      "phys_1_mean", "phys_2_mean", "phys_3_mean"
    ),
    currency = "",
    digits   = 1
  )

# ---- tables-annotation ----------------------------------------------------------
# ds_annotation %>%
#   DT::datatable(
#     colnames=gsub("_", " ", colnames(.)),
#     options = list(
#       pageLength = 16
#     )
#   )


# ---- spaghetti --------------------------------------------

cat("\n\n###Cog 1<br/><b>Manifest</b>\n\n")#Post will be added
# names(ds_client_week)
ds_county_year %>%
  dplyr::group_by(county)%>%
  # dplyr::mutate(
  #   mean_pre = mean(stress_score_pre, na.rm=T),
  #   count    = length(which(!is.na(stress_score_pre)))
  # )%>%
  # dplyr::ungroup()%>%
  plot_ly(
    x = ~year,
    y = ~cog_1_mean,
    type = 'scatter',
    mode = "markers+lines",
    text = ~paste(
      "<br>Mean Cog 1 Score: ",
      round(cog_1_mean, 3),
      "<br>County: ",
      county
    ),
    name = "Mean Scores"
  ) %>%
  # add_trace(type = "scatter", mode = "markers+lines")
  dplyr::ungroup() %>%
  layout(
    # title = "Cog 1",
    xaxis = list(title=NA),
    yaxis = list(
      title = "Cog 1",
      titlefont = list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f")
    ),
    shapes=list(
      # list(
      #   type='line', x0= as.Date("2017-07-15"), x1= as.Date("2017-07-15"),
      #   y0=0, y1=22, line=list(dash='line', width=.5)
      # ),
      # list(
      #   type='line', x0= as.Date("2018-01-15"), x1= as.Date("2018-01-15"),
      #   y0=0, y1=22, line=list(dash='line', width=.5)
      # ),
      # list(
      #   type='line', x0= as.Date("2002-02-15"), x1= as.Date("2002-02-15"),
      #   y0=0, y1=5, line=list(dash='line', width=.5)
      # )
    )
  )


cat("\n\n###Stress Activity - Success<br/><b>Process</b>\n
    \n")
#
# ---- marginals ---------------------------------------------------------------
cat("\n\n### Goals Met  <br/><b>Disruptor Measure</b>\n\n")
#
