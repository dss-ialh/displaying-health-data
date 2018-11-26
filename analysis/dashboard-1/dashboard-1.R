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
county_focus              <- 72L
base_size                 <- 14L


# path_in_annotation      <- "./data-public/raw/programs/cqi-annotation-example.csv"
# colors <- c('#0000ff','#ff8000','#ffff99',   '#ff0000' )
palette_county_dark   <- c("Muskogee"="#b0d794"  , "Oklahoma"="#83c1b2"  ,  "Tulsa"="#f4a971"  ) #http://colrd.com/image-dna/28023/
palette_county_light  <- c("Muskogee"="#b0d79433", "Oklahoma"="#83c1b233",  "Tulsa"="#f4a97133")

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

ds_county <-
  ds_county %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    cog       = cog_1_mean  + cog_2_mean  + cog_3_mean ,
    phys      = phys_1_mean + phys_2_mean + phys_3_mean,
    label     = sprintf("%s mean:\n%3.1f", county, cog)
  ) %>%
  dplyr::mutate(
    emphasis        = dplyr::if_else(county_id == county_focus, "focus", "background"),
    county_id       = factor(county_id)
  )

ds_county_year <-
  ds_county_year %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    emphasis        = dplyr::if_else(county_id == county_focus, "focus", "background"),
    county_id       = factor(county_id)
  )

# ---- headline-graph ----------------------------------------------------------
# cat("\n\n\n### Goals Status-- (ALL REPORTING PERIOD)\n\n\n")
ggplot(ds_county, aes(x=county, y=cog, label=label, color=county, fill=county)) +
  geom_bar(stat="identity") +
  geom_label(color="gray30", fill="#88888833", vjust=1.3) +
  scale_color_manual(values=palette_county_dark) +
  scale_fill_manual(values=palette_county_light) +
  theme_light() +
  theme(legend.position="none") +
  theme(panel.grid.major.x = element_blank()) +
  theme(axis.ticks.x=element_blank()) +
  labs(title="Cognitive Outcome by County", x=NULL, y="Cognitive Mean")

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

cat("\n\n### Cog 1<br/><b>County-Year</b>\n\n")

ds_county_year %>%
  dplyr::group_by(county) %>%
  plot_ly(
    x = ~year,
    y = ~cog_1_mean,
    type = 'scatter',
    mode = "markers+lines",
    color = ~county,
    colors = palette_county_dark,
    text = ~sprintf(
      "<br>For county %s during %4i,<br>the average Cog 1 score was %1.2f.",
      county, year, cog_1_mean
    )
  ) %>%
  # add_trace(type = "scatter", mode = "markers+lines")
  dplyr::ungroup() %>%
  layout(
    # showlegend = FALSE,
    legend = list(orientation = 'h'),
    xaxis = list(title=NA),
    yaxis = list(
      title = "Cog 1",
      titlefont = list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f"
      )
    )
  )



cat("\n\n### Cog 2<br/><b>County-Year</b>\n\n")
spaghetti_1(
  d                   = ds_county_year,
  response_variable   = "cog_1_mean",
  time_variable       = "year",
  color_variable      = "county",
  group_variable      = "county",
  facet_variable      = NULL,
  palette             = palette_county_dark,
  path_in_annotation  = NULL,
  width               = c("focus"=2, "background"=1),
  base_size           = 18
)

cat("\n\n### Cog 3<br/><b>County-Year</b>\n\n")
spaghetti_1(
  d                   = ds_county_year,
  response_variable   = "cog_3_mean",
  time_variable       = "year",
  color_variable      = "county",
  group_variable      = "county",
  facet_variable      = NULL,
  palette             = palette_county_dark,
  path_in_annotation  = NULL,
  width               = c("focus"=2, "background"=1),
  base_size           = 18
)

# ---- marginals ---------------------------------------------------------------
cat("\n\n### Goals Met  <br/><b>Disruptor Measure</b>\n\n")
#
