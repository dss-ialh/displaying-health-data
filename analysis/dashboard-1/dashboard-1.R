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
ds            <- readr::read_rds(config$path_mlm_1_derived)
ds_county     <- readr::read_rds(config$path_county_derived)
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
ds  %>%
   dplyr::arrange(desc(year), county) %>%
   dplyr::select(
     week, worker_name, stress_count, self_care_count, self_care_yes, client_count, mean_pre_stress_score, proportion_self_care_yes
   )%>%
   DT::datatable(
     colnames=gsub("_", " ", colnames(.)),
     options = list(
       pageLength = 16
     )
   ) %>%
  DT::formatCurrency(
    columns  = c(
      "mean_pre_stress_score",
      "proportion_self_care_yes"
    ),
    currency = "",
    digits   = 1
  )



# ---- tables-county ----------------------------------------------------------
ds_client_week  %>%
  dplyr::arrange(desc(week), worker_name) %>%
     dplyr::select(
       week,
       created_id,
       self_care_activity_since_last_visit,
       stress_first_date,
       stress_pre_index,
       stress_score_pre,
       stress_score_post,
       self_care_percentage_cum,
       -program_code
     )%>%
  DT::datatable(
    colnames=gsub("_", " ", colnames(.)),
    options = list(
      pageLength = 16
    )
  ) %>%
  DT::formatCurrency(
    columns  = c(
      "self_care_percentage_cum"
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


cat("\n\n###Mean PSS Scores - Pre<br/><b>Outcome</b>\n\n")#Post will be added
# names(ds_client_week)
ds_client_week%>%
  dplyr::group_by(week)%>%
  dplyr::mutate(
    mean_pre = mean(stress_score_pre, na.rm=T),
    count    = length(which(!is.na(stress_score_pre)))
  )%>%
  dplyr::ungroup()%>%
  plot_ly(x = ~week, y = ~stress_score_pre,
          type = 'scatter',
          text = ~paste("<br>Individual Score ",
                        stress_score_pre,
                        "<br>client ",
                        created_id),
          name = "Mean Scores")%>%
  dplyr::ungroup()%>%
  dplyr::group_by(week)%>%
  add_trace(y = ~mean_pre,
            type = 'scatter', mode = 'markers',
             line = list(color = colors[4]),
            text = ~paste("<br>Mean Weekly Scores (Pre Only) ",
                          mean_pre,
                          "<br>n = ",
                          count),
            size = ~count,
            name = "Mean Scores")%>%
  layout(
    title = "Pre scores on PSS",
    xaxis = list(title=""),
    yaxis = list(
      title = "PSS Score",
      titlefont = list(
        family = "Courier New, monospace",
        size = 18,
        color = "#7f7f7f")))
    #,
  #   shapes=list(
  #     list(type='line', x0= as.Date("2017-07-15"), x1= as.Date("2017-07-15"),
  #          y0=0, y1=22, line=list(dash='line', width=.5)),
  #     list(type='line', x0= as.Date("2018-01-15"), x1= as.Date("2018-01-15"),
  #          y0=0, y1=22, line=list(dash='line', width=.5)),
  #     list(type='line', x0= as.Date("2018-02-15"), x1= as.Date("2018-02-15"),
  #          y0=0, y1=22, line=list(dash='line', width=.5)))
  # )%>%
  # add_annotations(
  #   x=as.Date("2017-07-15")+5,  y=0.25,  showarrow = FALSE,
  #   text="1st Learning Session",
  #   textangle = 90,  opacity = 0.5,
  #   font = list(size = 10,color = "grey")
  # )%>%
  # add_annotations(
  #   x=as.Date("2018-01-15")+5,  y=0.25,  showarrow = FALSE,
  #   text="2nd Learning Session",
  #   textangle = 90, opacity = 0.5,
  #   font = list(size = 10, color = "grey"
  #   )
  # )%>%
  # add_annotations(
  #   x=as.Date("2018-02-15")+5, y=0.25,   showarrow = FALSE,
  #   text="PAT Bethany Cycle 2 Start", textangle = 90, opacity = 0.5,
  #   font = list(size = 10,color = "grey")
  # )



cat("\n\n###Stress Activity - Success<br/><b>Process</b>\n
    \n")
#
# ---- marginals ---------------------------------------------------------------
cat("\n\n### Goals Met  <br/><b>Disruptor Measure</b>\n\n")
#
