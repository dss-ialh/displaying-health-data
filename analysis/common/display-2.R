hs_table_by_source_measurement <- function(
  d,
  outcome_name,
  source_name     = "source",
  missing_labels  = c("unknown", "Unknown", "missing", "Missing")
) {

  outcome_name_eq <- rlang::enquo(outcome_name)
  # source_name_eq  <- rlang::enquo(source_name)
  # print(outcome_name_eq)
  # print(source_name_eq)
  # cat("## ")

  d_measurement <-
    d %>%
    dplyr::rename(
      a = !!outcome_name_eq
    ) %>%
    dplyr::mutate(
      nonmissing   = !is.na(a) & !(a %in% missing_labels)
      # nonmissing   = !is.na(!!outcome_name_eq)
    ) %>%
    # dplyr::count(!! source_name_eq, nonmissing) %>%
    dplyr::count(source, nonmissing) %>%
    tidyr::complete(source, nonmissing) %>%
    dplyr::mutate(
      nonmissing        = dplyr::if_else(nonmissing, "measurement_level_nonmissing", "measurement_level_missing"),
      n                 = dplyr::coalesce(n, 0L)
    ) %>%
    tidyr::spread(key=nonmissing, value=n)

  d_patient <-
    d %>%
    dplyr::rename(
      a = !!outcome_name_eq
    ) %>%
    dplyr::group_by(participant_master_id, source) %>%
    dplyr::summarize(
      nonmissing_any    = any(!is.na(a) & !(a %in% missing_labels))
      # missing       = any( is.na(a))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::count(source, nonmissing_any) %>%
    tidyr::complete(source, nonmissing_any) %>%
    dplyr::mutate(
      nonmissing_any    = dplyr::if_else(nonmissing_any, "client_level_nonmissing", "client_level_missing"),
      n                 = dplyr::coalesce(n, 0L)
    ) %>%
    tidyr::spread(key=nonmissing_any, value=n)

  row_count <- length(union(d_measurement$source, d_patient$source))

  d_measurement %>%
    dplyr::full_join(d_patient, by="source") %>%
    tibble::add_row(
      source = "Total",
      measurement_level_missing     = sum(.$measurement_level_missing   ),
      measurement_level_nonmissing  = sum(.$measurement_level_nonmissing),
      client_level_missing          = sum(.$client_level_missing        ),
      client_level_nonmissing       = sum(.$client_level_nonmissing     )
    ) %>%
    knitr::kable(
      format = "html",
      col.names     = gsub("^(measurement_level_|client_level_)", "", colnames(.)),
      # col.names     = c("source", "nonmissing", "missing", "nonmissing", "missing"),
      # col.names     = gsub("_", " ", colnames(.)),
      caption       = outcome_name,
      format.args = list(big.mark = ",")
    ) %>%
    kableExtra::kable_styling(
      bootstrap_options   = c("striped", "hover", "condensed", "responsive"),
       # position           = "float_left",
      full_width          = F
    ) %>%
    kableExtra::add_header_above(c(" " = 1, "measurement-level" = 2, "client-level" = 2)) %>%
    kableExtra::row_spec(row_count + 1, bold=T, color="#444", background = "#ddd")#, color="white", background = "#775")
}

palette_dose_cut_5 <- c(
  # http://colrd.com/image-dna/24016/
  "0 visits"         = "#bb977d",   # brown
  "1 visit"          = "#4aab5e",   # green
  "2-10 visits"      = "#28737c",   # blue-gray
  "11-30 visits"     = "#1765a2",   # blue
  "31+ visits"       = "#5addd7"    # light blue
)
label_dose_cut_5 <- sub("^(.+)\\svisits?$", "\\1", names(palette_dose_cut_5))
spaghetti_dose_age_1 <- function( d, outcome_variable, age_variable="demo_child_index_age_years", knot_count_age=3 ) {
  # browser()
  g <- d %>%
    tidyr::drop_na_("dose_hv_visit_count") %>%
    tidyr::drop_na_(outcome_variable) %>%
    # dplyr::slice(1:2000) %>%
    ggplot(aes_string(x="dose_hv_visit_count", y=outcome_variable, group="participant_master_id")) +
    geom_line(color="#33333322", na.rm=TRUE) +
    geom_point(color="#33333311", na.rm=TRUE) +
    facet_wrap("source") +
    repo_theme() +
    labs(y=NULL)

  g1 <- g +
    geom_smooth(aes(group=1), method="gam", formula=y ~ s(x, bs = "cs"), color="#008000", na.rm=TRUE)
  g2 <- g  %+%
    aes_string(x=age_variable, color="dose_hv_visit_count_cut_5") +
    geom_smooth(
      aes(group=dose_hv_visit_count_cut_5),
      method="gam",
      formula=y ~ s(x, bs = "cs", k = knot_count_age),
      fill="#88888801",
      na.rm=TRUE
    ) +
    scale_color_manual(
      labels  = label_dose_cut_5,
      values  = palette_dose_cut_5
    ) +
    guides(color = guide_legend(
      title = "Visits",
      title.position = "left",
      override.aes = list(alpha = 0, size=3),
      nrow=1
    )) +
    theme(legend.position=c(1, 1), legend.justification=c(1, 1)) +
    theme(legend.background = element_rect(fill="#22222233"))
    labs(color=NULL)

  # gridExtra::grid.arrange(
  #   g1,
  #   g2,
  #   left = grid::textGrob(label=outcome_variable, rot=90, gp=grid::gpar(col="#008000", cex=1.7)) #Sync this color with theme_book
  # )
  ggpubr::ggarrange(
    g1,
    g2,
    ncol = 1,
    nrow =2
  ) %>%
  ggpubr::annotate_figure(
    left = grid::textGrob(label=outcome_variable, rot=90, gp=grid::gpar(col="#008000", cex=1.7)) #Sync this color with theme_book
  )
}
# spaghetti_dose_age_1(ds, "income_h3")
