# knitr::stitch_rmd(script="./manipulation/osdh/personal/beasley/scribe-provider-week.R", output="./stitched-output/manipulation/osdh/personal/beasley/scribe-provider-week.md")
rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# * writes four files to disk:
#    * one record per model
#    * one record per program
#    * one record per program-month
#    * one record per provider-week
# * coalesces different sources for a variable (where possible)

# ---- load-sources ------------------------------------------------------------
source("./manipulation/osdh/ellis/common-ellis.R")
# base::source(file="./Dal/Osdh/Arch/benchmark-client-program-arch.R") #Load retrieve_benchmark_client_program

# ---- load-packages -----------------------------------------------------------
library(magrittr, quietly=TRUE)
requireNamespace("DBI")
requireNamespace("odbc")
requireNamespace("dplyr")
requireNamespace("testit")
requireNamespace("lubridate")
requireNamespace("RcppRoll")
requireNamespace("OuhscMunge") # devtools::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
path_in_capacity          <- "data-public/raw/osdh/program-service-capacity.csv"
path_in_client_program    <- file.path(miechv3::path_scribe_personal("beasley"), "client-program.rds")

path_out_directory        <- miechv3::path_scribe_personal("beasley")
path_out_model            <- file.path(path_out_directory, "model.rds")
path_out_program          <- file.path(path_out_directory, "program.rds")
path_out_program_month    <- file.path(path_out_directory, "program-month.rds")
path_out_provider_week    <- file.path(path_out_directory, "provider-week.rds")

desired_models            <- miechv3::possible_model_names() #c("C1", "SC", "PAT") # "Template", "HFA", "P4"
programs_to_exclude       <- c(764) # NorthCare SafeCare
program_code_cherokee     <- 219L

# range_visit               <- c(as.Date("2015-01-01"), lubridate::floor_date(dump_date_eto(), unit="month")- lubridate::days(1))
range_visit               <- c(as.Date("2015-01-01"), OuhscMunge::clump_week_date(dump_date_eto()))
complete_month_last       <- lubridate::floor_date(Sys.Date(), unit="month")- lubridate::days(1)
# complete_week_first     <- lubridate::floor_date(Sys.Date(), unit="month")- lubridate::days(1)

activity_grace_period     <- 30L # Thirty days

ds_possible_month <- OuhscMunge::clump_month_date(seq.Date(range_visit[1], range_visit[2] , by="month")) %>%
  tibble::tibble("month" = .) %>%
  dplyr::filter(dplyr::between(month, range_visit[1], complete_month_last))

ds_possible_week <- OuhscMunge::clump_week_date(seq.Date(range_visit[1], range_visit[2] , by="week")) %>%
  tibble::tibble("week" = .) %>%
  dplyr::filter(dplyr::between(week, range_visit[1], range_visit[2]))

sql_visit <- "
  SELECT
    v.case_number
    ,v.program_code
    ,v.date_taken           AS visit_date
    ,v.completed            AS visit_completed
    ,v.brain_builder_video_shown
    ,v.worker_name
    ,p.county_id
    ,m.model_name_short     AS model_name
    --,v.model_id
    --,visit_distance
    --,visit_duration_in_minutes
    --,time_frame
    --,people_present_count
    --,visit_location_home
    --,content_covered_percent
    --,client_involvement
    --,client_material_conflict
    --,client_material_understanding
  FROM Osdh.tbl_eto_touchpoint_encounter v
    LEFT JOIN Osdh.tbl_lu_program p on v.program_code = p.program_code
    LEFT JOIN Osdh.tbl_lu_model   m on p.model_id     = m.model_id
  WHERE m.model_name_short IN ('C1', 'HFA', 'SC', 'PAT', 'P4')
  -- WHERE p.miechv_3 = 1
"

sql_addressable_not <- "
  SELECT
    case_number
    ,program_code
    ,program_date_end
    ,addressable_c1_tulsa  AS attrition_addressable
  FROM osdh.tbl_eto_enroll
  WHERE
	program_date_end IS NOT NULL
	AND
	addressable_c1_tulsa = 0
  ORDER BY case_number, program_code, program_date_end
"

col_types_capacity <- readr::cols_only(
  program_code          = readr::col_integer(),
  program_name_ugly     = readr::col_character(),
  county_id             = readr::col_integer(),
  service_capacity_count= readr::col_integer()
)

# ---- load-data ---------------------------------------------------------------
ds_lu_program   <- retrieve_program()
channel         <- open_dsn_channel_odbc()
ds_visit        <- DBI::dbGetQuery(channel, sql_visit)
ds_addressable_not <- DBI::dbGetQuery(channel, sql_addressable_not)
DBI::dbDisconnect(channel); rm(channel, sql_visit, sql_addressable_not)

ds_cherokee_month <- OuhscMunge::execute_sql_file(miechv3::config_value("query_cherokee_month"), miechv3::config_value("dsn_miechv"), execute=F)

checkmate::assert_data_frame(ds_visit)
checkmate::assert_data_frame(ds_cherokee_month, min.rows = 1)
checkmate::assert_data_frame(ds_addressable_not)

ds_program_capacity     <- readr::read_csv(path_in_capacity, col_types=col_types_capacity, comment="#")
ds_client_program       <- readr::read_rds(path_in_client_program)

rm(path_in_capacity, col_types_capacity, path_in_client_program)

# ---- tweak-data --------------------------------------------------------------
dim(ds_client_program)
ds_client_program <- ds_client_program %>%
  # dplyr::filter(miechv_3) %>%
  dplyr::filter(model_name %in% desired_models) %>%
  dplyr::filter(!(program_code %in% programs_to_exclude))
dim(ds_client_program)

ds_addressable_not <-
  ds_addressable_not %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    program_date_month  = OuhscMunge::clump_month_date(program_date_end)
    # attrition_addressable = as.logical(attrition_addressable)
  ) %>%
  dplyr::group_by(case_number, program_code, program_date_month) %>%
  dplyr::summarize(
    any_addressable_not = any(!attrition_addressable)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter()

# ds_client_program_758 <- ds_client_program %>%
#   dplyr::filter(program_code==758L) %>%
#   dplyr::filter(as.Date("2017-09-01") <= visit_program_month_completed_min & visit_program_month_completed_min<=as.Date("2017-10-01") )



# OuhscMunge::match_statistics(ds_lu_program   , ds_client_program              , c("program_code"))
# testit::assert("All programs should be matched.", OuhscMunge::match_statistics(ds_lu_program, ds_client_program, c("program_code"))[["child_not_in_parent"]]==0L)

dim(ds_visit)
ds_visit <- ds_visit %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    visit_completed       = as.logical(visit_completed),
    brain_builder_video_shown       = as.logical(brain_builder_video_shown),
    visit_month           = OuhscMunge::clump_month_date(visit_date),
    visit_week            = OuhscMunge::clump_week_date(visit_date),
    worker_name           = dplyr::coalesce(worker_name, "Unknown Worker")
  ) %>%
  dplyr::filter(model_name %in% desired_models) %>%
  dplyr::filter(!(program_code %in% programs_to_exclude))
dim(ds_visit)
dplyr::n_distinct(ds_visit$program_code)

ds_cherokee_month <- ds_cherokee_month %>%
  dplyr::mutate(
    model_name        = factor(model_name, levels=desired_models)
  )

# ds_client <- ds_visit %>%
#   dplyr::group_by(case_number, program_code) %>%
#   dplyr::filter(visit_completed) %>%
#   dplyr::summarize(
#     worker_name_last    = dplyr::last(worker_name, order_by=visit_date),
#     visit_first         = min(visit_date),
#     visit_last          = max(visit_date)
#   ) %>%
#   dplyr::ungroup()
# dim(ds_client)

# Wait to filter the dates until the client's first/last visit is calculated
ds_visit <- ds_visit %>%
  dplyr::filter(dplyr::between(visit_date, range_visit[1], range_visit[2]))
dim(ds_visit)
dplyr::n_distinct(ds_visit$program_code)


dim(ds_lu_program)
ds_lu_program <- ds_lu_program %>%
  # dplyr::filter(miechv_3==1L) %>%
  dplyr::filter(!(program_code %in% programs_to_exclude)) %>%
  dplyr::select(program_code, program_name, model_name, model_id, miechv_3) %>%
  dplyr::filter(model_name %in% desired_models) %>%
  dplyr::mutate(
    miechv_3          = as.logical(miechv_3),
    model_name        = factor(model_name, levels=desired_models)
  ) %>%
  dplyr::left_join(
    ds_program_capacity %>%
      dplyr::select(program_code, service_capacity_count),
    by = "program_code"
  )
  # dplyr::semi_join(ds_client_program, "program_code")
dim(ds_lu_program)

# checkmate::assert_integer(ds_lu_program$service_capacity_count, any.missing=F)

ds_lu_worker <- ds_visit %>%
  dplyr::distinct(worker_name, program_code, model_name) %>%
  dplyr::filter(model_name %in% desired_models) %>%
  dplyr::mutate(
    model_name        = factor(model_name, levels=desired_models)
  ) %>%
  dplyr::left_join(
    ds_lu_program %>%
      dplyr::select(program_code, model_name, model_id, program_name),
    by = c("program_code", "model_name")
  ) %>%
  dplyr::arrange(worker_name, program_code)
dim(ds_lu_worker)
dplyr::n_distinct(ds_lu_worker$program_code)

# ---- possible-rows -----------------------------------------------------------
ds_possible_program_month <- ds_lu_program %>%
  dplyr::select(program_code, program_name, model_name, model_id, service_capacity_count) %>%
  tidyr::crossing(ds_possible_month)

ds_possible_program_week <- ds_lu_program %>%
  dplyr::select(program_code, program_name, model_name, model_id, service_capacity_count) %>%
  tidyr::crossing(ds_possible_week)


# ---- identify-conversions ----------------------------------------------------
# table(ds_client_program$convert_referral_to_enroll)
# table(ds_client_program[, c("referral_accepted_guess", "convert_referral_to_enroll")], useNA="always")
# a <- ds_visit %>% dplyr::filter(case_number %in% c(40130, 40801))

ds_client_conversion <- ds_client_program %>%
  # dplyr::filter(case_number %in% c(15346, 33858, 44093, 44130, 44103)) %>%
  dplyr::select(
    case_number,
    # worker_name,
    program_code,
    week                           = referral_week_guess,
    convert_referral_to_enroll,
    convert_enroll_to_visit_2,
    visit_first_is_alternative
  ) %>%
  tidyr::drop_na(case_number, program_code, week) %>%
  dplyr::mutate(
    month             = OuhscMunge::clump_month_date(week)
  ) %>%
  dplyr::left_join(
    ds_visit %>%
      dplyr::select(case_number, program_code, worker_name, visit_date) %>%
      dplyr::group_by(case_number, program_code) %>%
      dplyr::arrange(visit_date) %>%
      dplyr::summarize(
        worker_name   = OuhscMunge::first_nonmissing(worker_name)
      ) %>%
      dplyr::ungroup(),
    by = c("case_number", "program_code")
  )

# stop("Connect the clients w.o a visit to a provider")


# table(ds_client_program$convert_referral_to_enroll)
# table(ds_client_conversion$convert_referral_to_enroll)

ds_program_month_conversion <- ds_client_conversion %>%
  # dplyr::filter(program_code==751L) %>%
  dplyr::group_by(program_code, month) %>%
  dplyr::summarize(
    convert_referral_to_enroll_proportion         = mean(convert_referral_to_enroll, na.rm=T),
    convert_referral_to_enroll_count              = sum( convert_referral_to_enroll, na.rm=T),
    convert_referral_to_enroll_denominator        = sum(!is.na(convert_referral_to_enroll)),
    convert_enroll_to_visit_2_proportion          = mean(convert_enroll_to_visit_2, na.rm=T),
    convert_enroll_to_visit_2_count               = sum( convert_enroll_to_visit_2, na.rm=T),
    convert_enroll_to_visit_2_denominator         = sum(!is.na(convert_enroll_to_visit_2)),
    visit_first_is_alternative_count              = sum( visit_first_is_alternative, na.rm=T)
  ) %>%
  dplyr::ungroup()

# d <- ds_program_month_conversion %>%
#   dplyr::filter(program_code == 751L)
# convert_referral_to_enroll_proportion
# table(ds_client_conversion$convert_referral_to_enroll)
# plot(ds_program_month_conversion$convert_referral_to_enroll_denominator, ds_program_month_conversion$convert_referral_to_enroll_proportion)
# plot(ds_program_month_conversion$convert_referral_to_enroll_denominator, ds_program_month_conversion$convert_referral_to_enroll_count)

ds_provider_week_conversion <- ds_client_conversion %>%
  # dplyr::filter(program_code==751L) %>%
  dplyr::group_by(worker_name, program_code, week) %>%
  dplyr::summarize(
    convert_referral_to_enroll_proportion         = mean(convert_referral_to_enroll, na.rm=T),
    convert_referral_to_enroll_count              = sum( convert_referral_to_enroll, na.rm=T),
    convert_referral_to_enroll_denominator        = sum(!is.na(convert_referral_to_enroll)),
    convert_enroll_to_visit_2_proportion          = mean(convert_enroll_to_visit_2, na.rm=T),
    convert_enroll_to_visit_2_count               = sum( convert_enroll_to_visit_2, na.rm=T),
    convert_enroll_to_visit_2_denominator         = sum(!is.na(convert_enroll_to_visit_2)),
    visit_first_is_alternative_count              = sum( visit_first_is_alternative, na.rm=T)
  ) %>%
  dplyr::ungroup()

table(ds_provider_week_conversion$convert_referral_to_enroll_proportion)
plot(ds_provider_week_conversion$convert_referral_to_enroll_denominator, ds_provider_week_conversion$convert_referral_to_enroll_proportion)
plot(ds_provider_week_conversion$convert_referral_to_enroll_denominator, ds_provider_week_conversion$convert_referral_to_enroll_count)


# a <- ds_provider_week_conversion %>%
#   dplyr::filter(program_code==751L)
# %>%
#   dplyr::select(
#     convert_referral_to_visit_1
#   )

# ---- summarize-program-month-level --------------------------------------------------
# ds_program_month_referral <- ds_client_program %>%
#   dplyr::group_by(program_code, referral_month) %>%
#   dplyr::summarize(
#     referral_accepted_count = sum(referral_accepted_guess)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(month=referral_month)
# dim(ds_program_month_referral)
#
# ds_program_month_enroll <- ds_client_program %>%
#   dplyr::group_by(program_code, visit_program_month_min) %>%
#   dplyr::summarize(
#     enroll_count = n()
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::rename(month=visit_program_month_min)
# dim(ds_program_month_referral)

ds_program_month_visit <- ds_visit %>%
  dplyr::group_by(program_code, visit_month) %>%
  dplyr::summarize(
    visit_completed_count     = sum(visit_completed),
    visit_scheduled_count     = n(),
    visit_completed_per_scheduled = ( visit_completed_count / visit_scheduled_count),
    brain_video_shown_count   = sum(brain_builder_video_shown, na.rm=T),
    brain_video_chance_count  = sum(!is.na(brain_builder_video_shown)),
    brain_video_shown_proportion = brain_video_shown_count / brain_video_chance_count
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(month=visit_month)
dim(ds_program_month_visit)

ds_provider_week_visit <- ds_visit %>%
  dplyr::group_by(program_code, worker_name, visit_week) %>%
  dplyr::summarize(
    visit_completed_count     = sum(visit_completed),
    visit_scheduled_count     = n(),
    visit_completed_per_scheduled = ( visit_completed_count / visit_scheduled_count),
    brain_video_shown_count   = sum(brain_builder_video_shown, na.rm=T),
    brain_video_chance_count  = sum(!is.na(brain_builder_video_shown)),
    brain_video_shown_proportion = brain_video_shown_count / brain_video_chance_count
  ) %>%
  dplyr::ungroup() %>%
  dplyr::rename(week=visit_week)
dim(ds_provider_week_visit)


# ---- case-load ---------------------------------------------------------------
ds_case_load_program_month <- ds_possible_program_month %>%
  dplyr::left_join(
    ds_client_program %>%
      dplyr::select(program_code, visit_program_week_completed_min, visit_program_week_completed_max), # miechv_3
    by=c("program_code")
  ) %>%
  tidyr::drop_na(visit_program_week_completed_min, visit_program_week_completed_max) %>%
  dplyr::mutate(
    visit_last_grade_period   = (visit_program_week_completed_max + lubridate::days(activity_grace_period)),
    active_in_period          = (visit_program_week_completed_min<=month & month<=visit_last_grade_period)
  ) %>%
  dplyr::filter(active_in_period) %>%
  dplyr::group_by(program_code, month) %>%  # , model_id, model_name, miechv_3
  dplyr::summarize(
    case_load_count               = sum(active_in_period),
    # service_capacity_count      = mean(service_capacity_count),
    case_capacity_proportion      = case_load_count / mean(service_capacity_count)
  ) %>%
  dplyr::ungroup()
dplyr::n_distinct(ds_case_load_program_month$program_code)

ds_case_load_program_month
table(is.na(ds_case_load_program_month$case_capacity_proportion))
# table(is.na(ds_case_load_program_month$case_capacity_proportion), ds_case_load_program_month$miechv_3)
# checkmate::assert_numeric(ds_case_load_program_month$case_capacity_proportion[ds_case_load_program_month$miechv_3], any.missing=F)

ds_case_load_provider_week <- ds_lu_worker %>%
  tidyr::crossing(ds_possible_week) %>%
  dplyr::left_join(
    ds_client_program %>%
      dplyr::select(program_code, worker_name_last, visit_program_week_completed_min, visit_program_week_completed_max),
    by=c("program_code", "worker_name"="worker_name_last")
  ) %>%
  tidyr::drop_na(visit_program_week_completed_min, visit_program_week_completed_max) %>%
  # dplyr::filter(any(dplyr::between(week, visit_first, visit_last))) %>%
  dplyr::mutate(
    visit_last_grade_period   = (visit_program_week_completed_max + lubridate::days(activity_grace_period)),
    # active_in_period        = (dplyr::between(week, visit_first, visit_last))
    active_in_period          = (visit_program_week_completed_min<=week & week<=visit_last_grade_period)
  ) %>%
  dplyr::filter(active_in_period) %>%
  dplyr::group_by(program_code, worker_name, week) %>%  # , model_id, model_name
  dplyr::summarize(
    case_load_count           = sum(active_in_period)
  ) %>%
  dplyr::ungroup()

# library(ggplot2)
# ggplot(ds_case_load_provider_week, aes(x=week, y=active_count, color=model_name)) +
#   geom_line(aes(group=worker_name)) #+
#   # theme(legend.position="none")
# rm(ds_client)
purrr::map(ds_case_load_provider_week, ~sum(is.na(.)))


# ---- visit-rolling-provider-week -----------------------------------------------------------
ds_possible_client_week <- ds_client_program %>%
  dplyr::distinct(case_number, program_code, visit_program_week_completed_min, visit_program_week_completed_max, worker_name_last) %>%
  tidyr::crossing(ds_possible_week) %>%
  dplyr::filter(visit_program_week_completed_min<=week & week<=visit_program_week_completed_max) %>%
  dplyr::select(
    case_number,
    program_code,
    worker_name_last,
    week_start_inclusive      = week
  ) %>%
  dplyr::mutate(
    week_stop_exclusive = week_start_inclusive + lubridate::days(7)
  )

ds_client_week_visit_goal <- "
  SELECT
    p.case_number,
    p.program_code,
    p.worker_name_last                AS worker_name,
    p.week_start_inclusive,
    --COUNT(v.visit_date)              AS visit_week_scheduled_count,
    COALESCE(SUM(v.visit_completed), 0)           AS visit_week_completed_count
  FROM ds_possible_client_week p
    LEFT JOIN ds_visit v ON (
      p.case_number=v.case_number
      AND
      p.program_code=v.program_code
      AND
      v.visit_date BETWEEN p.week_start_inclusive AND p.week_stop_exclusive
      -- strftime('%s', v.visit_date) BETWEEN strftime('%s', p.week_start_inclusive) AND strftime('%s', p.week_stop_exclusive)
      -- (p.week_start_inclusive <= v.visit_date AND v.visit_date<p.week_stop_exclusive)
    )
  GROUP BY p.case_number, p.program_code, p.worker_name_last, p.week_start_inclusive, p.week_stop_exclusive
  ORDER BY p.case_number, p.program_code, p.worker_name_last, p.week_start_inclusive, p.week_stop_exclusive
" %>%
  sqldf::sqldf() %>%
  tibble::as_tibble() %>%
  # dplyr::right_join(
  #   ds_possible_client_week %>%
  #     dplyr::select(case_number, program_code, week_start_inclusive, week_stop_exclusive),
  #     # dplyr::select(case_number, program_code, week_start_inclusive),
  #
  #   by = c("case_number", "program_code", "week_start_inclusive")
  # ) %>%
  dplyr::left_join(
    ds_lu_program %>%
      dplyr::select(program_code, model_name),
    by = c("program_code")
  ) %>%
  dplyr::left_join(
    ds_client_program %>%
      dplyr::select(case_number, program_code, child_mob_guess, visit_program_week_completed_min),
    by = c("case_number", "program_code")
  ) %>%
  dplyr::mutate(
    visit_week_completed_count  = dplyr::coalesce(visit_week_completed_count, 0L),
    model_name                  = as.character(model_name),
    first_weeks_12              = (week_start_inclusive <= visit_program_week_completed_min + lubridate::weeks(12))
  ) %>%
  dplyr::rename(
    # worker_name         = worker_name_last,
    week                = week_start_inclusive
  ) %>%
  dplyr::group_by(program_code, worker_name, case_number) %>%
  dplyr::mutate(
    enroll_duration_days      = as.integer(difftime(week, dplyr::coalesce(visit_program_week_completed_min, min(week)), units="days")),
    index_child_age_days      = as.integer(difftime(week, child_mob_guess, units="days")),
    visit_count_past_3_weeks  = RcppRoll::roll_sum(visit_week_completed_count, 3, fill=NA_integer_, align="right"),
    visit_count_past_4_weeks  = RcppRoll::roll_sum(visit_week_completed_count, 4, fill=NA_integer_, align="right"),
    visit_goal                = expected_visits_monthly(model_name, enroll_duration_days, index_child_age_days),
    reached_visit_goal        = (visit_goal <= visit_count_past_4_weeks)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(case_number, program_code) %>%
  dplyr::arrange(case_number, program_code, week) %>%
  dplyr::mutate(
    client_week_index                      = seq_len(n())
  ) %>%
  dplyr::ungroup() %>%
  dplyr::select(-model_name)

dplyr::n_distinct(ds_client_week_visit_goal$program_code)

ds_provider_week_visit_goal <- ds_client_week_visit_goal %>%
  dplyr::group_by(program_code, worker_name, week) %>%
  dplyr::summarize(
    scheduled_unique_client_count = dplyr::n_distinct(case_number),
    visit_count_past_4_weeks_mean = mean(visit_count_past_4_weeks, na.rm=T),

    # reached_goal_numerator        = sum(reached_visit_goal, na.rm=T),
    # reached_goal_denominator      = sum(!is.na(reached_visit_goal)),
    reached_goal_proportion       = mean(reached_visit_goal, na.rm=T)
  ) %>%
  dplyr::ungroup()


# ds_client_week_visit_goal %>%
#   dplyr::filter(enroll_duration_days <= 120L)

# table(ds_client_week_visit_goal$model_name, is.na(ds_client_week_visit_goal$index_child_age_days))

# ds_client_week_visit_goal %>%
#   dplyr::group_by(model_name) %>%
#   dplyr::summarize(
#     reached_goal_proportion = mean(reached_visit_goal, na.rm=T)
#   ) %>%
#   dplyr::ungroup()

# a <- ds_client_week_visit_goal %>%
#   dplyr::filter(case_number == 40957L)

# ds_program_month_visit_goal_granular <- ds_client_week_visit_goal %>%
#   dplyr::mutate(
#     month   = OuhscMunge::clump_month_date(week)
#   ) %>%
#   dplyr::group_by(program_code, month) %>%
#   dplyr::summarize(
#     scheduled_unique_client_count = dplyr::n_distinct(case_number),
#     visit_count_past_4_weeks_mean = mean(visit_count_past_4_weeks, na.rm=T),
#     reached_goal_proportion       = mean(reached_visit_goal, na.rm=T)
#   ) %>%
#   dplyr::ungroup()
#
# dplyr::n_distinct(ds_program_month_visit_goal_granular$program_code)
# #
# mean(ds_program_month_visit_goal_granular$reached_goal_proportion, na.rm=T)
# sd(ds_program_month_visit_goal_granular$reached_goal_proportion, na.rm=T)
# fivenum(ds_program_month_visit_goal_granular$reached_goal_proportion, na.rm=T)



# library(ggplot2)
# ggplot(ds_provider_week, aes(x=client_count, y=case_load_count)) +
#   geom_point(position=position_jitter(), alpha=.02) +
#   geom_abline(color="tomato") +
#   theme_light()

# ds_client_week <- "
#   SELECT
#     p.case_number,
#     p.week_start_inclusive,
#     COUNT(v.visit_date)            AS visit_count
#   FROM ds_possible_client_week p
#     INNER JOIN ds_visit v ON (
#       p.case_number=v.case_number
#       AND
#       (p.week_start_inclusive <= v.visit_date AND v.visit_date<p.week_stop_exclusive)
#     )
#   WHERE v.visit_completed = 1
#   GROUP BY p.case_number, p.week_start_inclusive
#   ORDER BY p.case_number, p.week_start_inclusive
#
# " %>%
#   sqldf::sqldf() %>%
#   tibble::as_tibble()

# ---- visit-rolling-program-month -----------------------------------------------------
# This is the corasened version, where the month s chopped at the boundaries.
#   https://github.com/OuhscBbmc/miechv-3/issues/1001
#   The more granular measure is commented out, and lives in the previous chunk

ds_possible_client_month <- ds_client_program %>%
  dplyr::distinct(case_number, program_code, visit_program_month_completed_min, visit_program_month_completed_max, worker_name_last) %>%
  tidyr::crossing(ds_possible_month) %>%
  dplyr::filter(visit_program_month_completed_min<=month & month<=visit_program_month_completed_max) %>%
  dplyr::select(
    case_number,
    program_code,
    worker_name_last,
    month
  )
dplyr::n_distinct(ds_possible_client_month$program_code)

ds_client_month_visit_goal <- "
  SELECT
    p.case_number,
    p.program_code,
    v.model_name,
    p.worker_name_last                    AS worker_name,
    p.month,
    COALESCE(SUM(v.visit_completed), 0)   AS visit_completed_count
    --r.child_mob_guess,
    --r.visit_program_week_completed_min

  FROM ds_possible_client_month p
    LEFT JOIN ds_visit          v ON (p.case_number=v.case_number AND p.program_code=v.program_code AND v.visit_month=p.month)
    LEFT JOIN ds_client_program r ON (p.case_number=r.case_number AND p.program_code=r.program_code)

  GROUP BY p.case_number, p.program_code, v.model_name, p.worker_name_last, p.month --, r.child_mob_guess, r.visit_program_week_completed_min
  ORDER BY p.case_number, p.program_code, v.model_name, p.worker_name_last, p.month --, r.child_mob_guess, r.visit_program_week_completed_min
" %>%
  sqldf::sqldf() %>%
  tibble::as_tibble() %>%
  dplyr::left_join(
    ds_client_program %>%
      dplyr::select(case_number, program_code, child_mob_guess, visit_program_week_completed_min),
    by = c("case_number", "program_code")
  ) %>%
  dplyr::group_by(program_code, worker_name, case_number) %>%
  dplyr::mutate(
    enroll_duration_days      = max(0L, as.integer(difftime(month, dplyr::coalesce(visit_program_week_completed_min, min(month)), units="days"))),
    index_child_age_days      = as.integer(difftime(month, child_mob_guess, units="days")),
    visit_goal                = expected_visits_monthly(model_name, enroll_duration_days, index_child_age_days),
    reached_visit_goal        = (visit_goal <= visit_completed_count)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::arrange(case_number, program_code, month) #%>% dplyr::select(-model_name)

sum(duplicated(paste(ds_client_month_visit_goal$case_number, ds_client_month_visit_goal$program_code, ds_client_month_visit_goal$month)))


ds_program_month_visit_goal <- ds_client_month_visit_goal %>%
  dplyr::left_join(ds_addressable_not, by=c("case_number", "program_code", "month"="program_date_month")) %>%
  dplyr::filter(is.na(any_addressable_not)) %>%
  # Remove client's month if they attrited w/ an addressable reason that month
  # left join (on tbl_enroll), then filter
  dplyr::group_by(program_code, month) %>%
  dplyr::summarize(
    scheduled_unique_client_count = dplyr::n_distinct(case_number),
    # visit_completed_count         = mean(visit_completed_count, na.rm=T),
    reached_goal_proportion       = mean(reached_visit_goal, na.rm=T)
  ) %>%
  dplyr::ungroup()

dplyr::n_distinct(ds_program_month_visit_goal$program_code)

# Considering addressable attrition improves the core measure from 0.542944 to 0.5453139
mean(ds_program_month_visit_goal$reached_goal_proportion, na.rm=T)

# ---- examine-756 -------------------------------------------------------------
ds_examine <- ds_client_week_visit_goal %>%
  dplyr::filter(program_code == 756L) %>%
  # dplyr::filter(as.Date("2018-04-02") < week & week <= as.Date("2018-04-30")) %>%
  dplyr::filter(as.Date("2018-03-12") < week & week <= as.Date("2018-04-09")) %>%
  dplyr::filter(worker_name == "Lucero, Yolanda") %>%
  # dplyr::filter(case_number %in% c(20542)) %>%
  dplyr::select(case_number, week, visit_week_completed_count, visit_count_past_3_weeks, visit_count_past_4_weeks, reached_visit_goal, worker_name) %>%
  # dplyr::arrange(dplyr::desc(week), worker_name, case_number)
  dplyr::arrange(case_number, dplyr::desc(week))

dplyr::n_distinct(ds_examine$case_number)

ds_examine_2 <- ds_provider_week_visit_goal %>%
  dplyr::filter(program_code == 756L) %>%
  # dplyr::filter(worker_name == "Njikam, Kethzia") %>%
  # dplyr::filter(worker_name == "Kyser, Maria") %>%
  # dplyr::filter(worker_name == "Mas, Rossana") %>%
  # dplyr::filter(worker_name == "Lucero, Yolanda") %>%
  dplyr::arrange(dplyr::desc(week)) %>%
  print(n=35)
# readr::

ds_examine_3 <- ds_program_month_visit_goal %>%
  dplyr::filter(program_code == 756L) %>%
  dplyr::arrange(dplyr::desc(month))


# program 756 workers ending April 30, 2018 0.8030986 = (.92*26 + .72*11 + .82*24 + .55*10) / (26+11+24+10)
# program 756 workers ending April 23, 2018 0.8518571 = (.96*25 + .81*11 + .93*24 + .44*10) / (25+11+24+10)
# program 756 workers ending April 16, 2018 0.7573611 = (.88*25 + .81*11 + .67*26 + .62*10) / (25+11+26+10)
# program 756 workers ending April  9, 2018 0.4400000 = (.40*25 + .09*11 + .61*25 + .50*10) / (25+11+25+10)
# program 756 workers ending April  1, 2018 0.4541429 = (.40*25 + .36*11 + .67*25 + .12* 9) / (25+11+25+ 9)

mean(c(0.8030986, 0.8518571, 0.7573611, 0.4400000, 0.4541429)) # = 0.6612919



# ---- first-12-weeks ---------------------------------------------------------
ds_client_visit_cum <- ds_visit %>%
  dplyr::rename(week = visit_week) %>%
  dplyr::group_by(case_number, program_code, worker_name, week) %>%
  dplyr::summarize(
    brain_builder_video_shown   = any(brain_builder_video_shown, na.rm = T)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(
    ds_client_week_visit_goal %>%
      dplyr::select(case_number, program_code, week, client_week_index),
    by=c("case_number", "program_code", "week")
  ) %>%
  dplyr::group_by(case_number, program_code) %>%
  tidyr::fill(worker_name) %>%
  dplyr::mutate(
    brain_builder_video_shown_cum   = dplyr::cumany(dplyr::coalesce(brain_builder_video_shown, FALSE)),
    first_weeks_12                  = (client_week_index <= 12L)
  ) %>%
  dplyr::ungroup() #%>% dplyr::filter(program_code == 751L)


ds_program_month_cum <- ds_client_visit_cum %>%
  dplyr::mutate(
    month   = OuhscMunge::clump_month_date(week)
  ) %>%
  dplyr::group_by(program_code, month) %>%
  dplyr::summarize(
    seen_video_in_12_weeks_count        = sum(brain_builder_video_shown_cum & first_weeks_12),
    first_weeks_12_count                = sum(first_weeks_12),
    seen_video_in_12_weeks_proportion   = seen_video_in_12_weeks_count / first_weeks_12_count
  ) %>%
  dplyr::ungroup()

ds_provider_week_cum <- ds_client_visit_cum %>%
  dplyr::group_by(program_code, worker_name, week) %>%
  dplyr::summarize(
    seen_video_in_12_weeks_count        = sum(brain_builder_video_shown_cum & first_weeks_12),
    first_weeks_12_count                = sum(first_weeks_12),
    seen_video_in_12_weeks_proportion   = seen_video_in_12_weeks_count / first_weeks_12_count
  ) %>%
  dplyr::ungroup()


# ---- referral-and-enroll -----------------------------------------------------
ds_client_program %>%
  dplyr::select(
    case_number,
    program_code,
    referral_month,
    enroll_month         = visit_program_month_completed_min
  ) %>%
  dplyr::mutate(
    referral_month       = dplyr::coalesce(referral_month, enroll_month) # Fill in the missing referral dates.
  )


# ---- retention ---------------------------------------------------------------
ds_program_month_retention <- ds_client_program %>%
  dplyr::select(
    case_number,
    program_code,
    # enroll_week         = visit_program_week_completed_min,
    month              = visit_program_month_completed_min,
    dplyr::matches("^retention_month_\\d{2}$")
  ) %>%
  tidyr::drop_na(month) %>%
  dplyr::group_by(program_code, month) %>%
  dplyr::summarize(
    retention_month_01_mean         = mean(retention_month_01, na.rm=T),
    retention_month_02_mean         = mean(retention_month_02, na.rm=T),
    retention_month_03_mean         = mean(retention_month_03, na.rm=T),
    retention_month_06_mean         = mean(retention_month_06, na.rm=T),
    retention_month_09_mean         = mean(retention_month_09, na.rm=T),
    retention_month_12_mean         = mean(retention_month_12, na.rm=T),
    retention_month_15_mean         = mean(retention_month_15, na.rm=T),
    retention_month_18_mean         = mean(retention_month_18, na.rm=T),
    retention_month_21_mean         = mean(retention_month_21, na.rm=T),
    retention_month_24_mean         = mean(retention_month_24, na.rm=T),

    retention_month_01_denominator  = sum(!is.na(retention_month_01)),
    retention_month_02_denominator  = sum(!is.na(retention_month_02)),
    retention_month_03_denominator  = sum(!is.na(retention_month_03)),
    retention_month_06_denominator  = sum(!is.na(retention_month_06)),
    retention_month_09_denominator  = sum(!is.na(retention_month_09)),
    retention_month_12_denominator  = sum(!is.na(retention_month_12)),
    retention_month_15_denominator  = sum(!is.na(retention_month_15)),
    retention_month_18_denominator  = sum(!is.na(retention_month_18)),
    retention_month_21_denominator  = sum(!is.na(retention_month_21)),
    retention_month_24_denominator  = sum(!is.na(retention_month_24))
    ) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(
    ds_possible_program_month %>%
      dplyr::select(program_code, month),
    by=c("program_code", "month")
  )

# # TODO: include a client's initial worker
ds_provider_week_retention <- ds_client_program %>%
  dplyr::select(
    case_number,
    program_code,
    worker_name       = worker_name_last,
    week              = referral_week,
    dplyr::matches("^retention_month_\\d{2}$")
  ) %>%
  tidyr::drop_na(week) %>%
  dplyr::group_by(program_code, worker_name, week) %>%
  dplyr::summarize(
    retention_month_01_mean         = mean(retention_month_01, na.rm=T),
    retention_month_02_mean         = mean(retention_month_02, na.rm=T),
    retention_month_03_mean         = mean(retention_month_03, na.rm=T),
    retention_month_06_mean         = mean(retention_month_06, na.rm=T),
    retention_month_09_mean         = mean(retention_month_09, na.rm=T),
    retention_month_12_mean         = mean(retention_month_12, na.rm=T),
    retention_month_15_mean         = mean(retention_month_15, na.rm=T),
    retention_month_18_mean         = mean(retention_month_18, na.rm=T),
    retention_month_21_mean         = mean(retention_month_21, na.rm=T),
    retention_month_24_mean         = mean(retention_month_24, na.rm=T),

    retention_month_01_denominator  = sum(!is.na(retention_month_01)),
    retention_month_02_denominator  = sum(!is.na(retention_month_02)),
    retention_month_03_denominator  = sum(!is.na(retention_month_03)),
    retention_month_06_denominator  = sum(!is.na(retention_month_06)),
    retention_month_09_denominator  = sum(!is.na(retention_month_09)),
    retention_month_12_denominator  = sum(!is.na(retention_month_12)),
    retention_month_15_denominator  = sum(!is.na(retention_month_15)),
    retention_month_18_denominator  = sum(!is.na(retention_month_18)),
    retention_month_21_denominator  = sum(!is.na(retention_month_21)),
    retention_month_24_denominator  = sum(!is.na(retention_month_24))
  ) %>%
  dplyr::ungroup() %>%
  dplyr::right_join(
    ds_provider_week_cum %>%
      dplyr::select(program_code, worker_name, week),
    by=c("program_code", "worker_name", "week")
  )

# retention_month_01

# ---- retention-shifted -------------------------------------------------------
ds_program_month_retention_shifted <- ds_program_month_retention %>%
  dplyr::group_by(program_code) %>%
  dplyr::mutate(
    retention_month_01_mean         = dplyr::lag(retention_month_01_mean          , n = 01L, order_by = month),
    retention_month_02_mean         = dplyr::lag(retention_month_02_mean          , n = 02L, order_by = month),
    retention_month_03_mean         = dplyr::lag(retention_month_03_mean          , n = 03L, order_by = month),
    retention_month_06_mean         = dplyr::lag(retention_month_06_mean          , n = 06L, order_by = month),
    retention_month_09_mean         = dplyr::lag(retention_month_09_mean          , n = 09L, order_by = month),
    retention_month_12_mean         = dplyr::lag(retention_month_12_mean          , n = 12L, order_by = month),
    retention_month_15_mean         = dplyr::lag(retention_month_15_mean          , n = 15L, order_by = month),
    retention_month_18_mean         = dplyr::lag(retention_month_18_mean          , n = 18L, order_by = month),
    retention_month_21_mean         = dplyr::lag(retention_month_21_mean          , n = 21L, order_by = month),
    retention_month_24_mean         = dplyr::lag(retention_month_24_mean          , n = 24L, order_by = month),

    retention_month_01_denominator  = dplyr::lag(retention_month_01_denominator   , n = 01L, order_by = month),
    retention_month_02_denominator  = dplyr::lag(retention_month_02_denominator   , n = 02L, order_by = month),
    retention_month_03_denominator  = dplyr::lag(retention_month_03_denominator   , n = 03L, order_by = month),
    retention_month_06_denominator  = dplyr::lag(retention_month_06_denominator   , n = 06L, order_by = month),
    retention_month_09_denominator  = dplyr::lag(retention_month_09_denominator   , n = 09L, order_by = month),
    retention_month_12_denominator  = dplyr::lag(retention_month_12_denominator   , n = 12L, order_by = month),
    retention_month_15_denominator  = dplyr::lag(retention_month_15_denominator   , n = 15L, order_by = month),
    retention_month_18_denominator  = dplyr::lag(retention_month_18_denominator   , n = 18L, order_by = month),
    retention_month_21_denominator  = dplyr::lag(retention_month_21_denominator   , n = 21L, order_by = month),
    retention_month_24_denominator  = dplyr::lag(retention_month_24_denominator   , n = 24L, order_by = month)
  ) %>%
  dplyr::ungroup()

month_to_week <- function( x ) {
  as.integer(x * 365.25/12/7)
}

ds_provider_week_retention_shifted <- ds_provider_week_retention %>%
  dplyr::group_by(program_code, worker_name) %>%
  dplyr::mutate(
    retention_month_01_mean         = dplyr::lag(retention_month_01_mean          , n = month_to_week(01L), order_by = week),
    retention_month_02_mean         = dplyr::lag(retention_month_02_mean          , n = month_to_week(02L), order_by = week),
    retention_month_03_mean         = dplyr::lag(retention_month_03_mean          , n = month_to_week(03L), order_by = week),
    retention_month_06_mean         = dplyr::lag(retention_month_06_mean          , n = month_to_week(06L), order_by = week),
    retention_month_09_mean         = dplyr::lag(retention_month_09_mean          , n = month_to_week(09L), order_by = week),
    retention_month_12_mean         = dplyr::lag(retention_month_12_mean          , n = month_to_week(12L), order_by = week),
    retention_month_15_mean         = dplyr::lag(retention_month_15_mean          , n = month_to_week(15L), order_by = week),
    retention_month_18_mean         = dplyr::lag(retention_month_18_mean          , n = month_to_week(18L), order_by = week),
    retention_month_21_mean         = dplyr::lag(retention_month_21_mean          , n = month_to_week(21L), order_by = week),
    retention_month_24_mean         = dplyr::lag(retention_month_24_mean          , n = month_to_week(24L), order_by = week),

    retention_month_01_denominator  = dplyr::lag(retention_month_01_denominator   , n = month_to_week(01L), order_by = week),
    retention_month_02_denominator  = dplyr::lag(retention_month_02_denominator   , n = month_to_week(02L), order_by = week),
    retention_month_03_denominator  = dplyr::lag(retention_month_03_denominator   , n = month_to_week(03L), order_by = week),
    retention_month_06_denominator  = dplyr::lag(retention_month_06_denominator   , n = month_to_week(06L), order_by = week),
    retention_month_09_denominator  = dplyr::lag(retention_month_09_denominator   , n = month_to_week(09L), order_by = week),
    retention_month_12_denominator  = dplyr::lag(retention_month_12_denominator   , n = month_to_week(12L), order_by = week),
    retention_month_15_denominator  = dplyr::lag(retention_month_15_denominator   , n = month_to_week(15L), order_by = week),
    retention_month_18_denominator  = dplyr::lag(retention_month_18_denominator   , n = month_to_week(18L), order_by = week),
    retention_month_21_denominator  = dplyr::lag(retention_month_21_denominator   , n = month_to_week(21L), order_by = week),
    retention_month_24_denominator  = dplyr::lag(retention_month_24_denominator   , n = month_to_week(24L), order_by = week)
  ) %>%
  dplyr::ungroup()

rm(ds_program_month_retention, ds_provider_week_retention)

# ---- combine -----------------------------------------------------------------
program_month_keys            <- c("program_code", "month")
program_provider_week_keys    <- c("program_code", "worker_name" , "week")

ds_program_month <- ds_possible_program_month %>%
  # dplyr::left_join(ds_program_month_referral      , by=program_month_keys) %>%
  # dplyr::left_join(ds_program_month_enroll        , by=program_month_keys) %>%
  dplyr::left_join(ds_program_month_visit           , by=program_month_keys) %>%
  dplyr::left_join(ds_program_month_conversion      , by=program_month_keys) %>%
  dplyr::left_join(ds_case_load_program_month       , by=program_month_keys) %>%
  dplyr::left_join(ds_program_month_visit_goal      , by=program_month_keys) %>%
  dplyr::left_join(ds_program_month_cum             , by=program_month_keys) %>%
  dplyr::left_join(ds_program_month_retention_shifted , by=program_month_keys) %>%
  dplyr::mutate(
    # referral_accepted_count     = dplyr::coalesce(referral_accepted_count    , 0L),
    # enroll_count                = dplyr::coalesce(enroll_count               , 0L),
    visit_completed_count         = dplyr::coalesce(visit_completed_count      , 0L),
    visit_scheduled_count         = dplyr::coalesce(visit_scheduled_count      , 0L),
    case_load_count               = dplyr::coalesce(case_load_count            , 0L),

    brain_video_shown_count                   = dplyr::coalesce(brain_video_shown_count                   , 0L),
    brain_video_chance_count                  = dplyr::coalesce(brain_video_chance_count                  , 0L),
    convert_referral_to_enroll_count          = dplyr::coalesce(convert_referral_to_enroll_count          , 0L),
    convert_referral_to_enroll_denominator    = dplyr::coalesce(convert_referral_to_enroll_denominator    , 0L),
    convert_enroll_to_visit_2_count           = dplyr::coalesce(convert_enroll_to_visit_2_count           , 0L),
    convert_enroll_to_visit_2_denominator     = dplyr::coalesce(convert_enroll_to_visit_2_denominator     , 0L),
    visit_first_is_alternative_count          = dplyr::coalesce(visit_first_is_alternative_count          , 0L),
    scheduled_unique_client_count             = dplyr::coalesce(scheduled_unique_client_count             , 0L),

    retention_month_01_denominator            = dplyr::coalesce(retention_month_01_denominator            , 0L),
    retention_month_02_denominator            = dplyr::coalesce(retention_month_02_denominator            , 0L),
    retention_month_03_denominator            = dplyr::coalesce(retention_month_03_denominator            , 0L),
    retention_month_06_denominator            = dplyr::coalesce(retention_month_06_denominator            , 0L),
    retention_month_09_denominator            = dplyr::coalesce(retention_month_09_denominator            , 0L),
    retention_month_12_denominator            = dplyr::coalesce(retention_month_12_denominator            , 0L),
    retention_month_15_denominator            = dplyr::coalesce(retention_month_15_denominator            , 0L),
    retention_month_18_denominator            = dplyr::coalesce(retention_month_18_denominator            , 0L),
    retention_month_21_denominator            = dplyr::coalesce(retention_month_21_denominator            , 0L),
    retention_month_24_denominator            = dplyr::coalesce(retention_month_24_denominator            , 0L)
  ) %>%
  dplyr::filter(program_code != unique(ds_cherokee_month$program_code)) %>% #Remove the Cherokee rows created by the "possible"
  dplyr::union_all(ds_cherokee_month) %>%
  dplyr::left_join(
    ds_lu_program %>%
      dplyr::select(program_code, miechv_3),
    by = "program_code"
  )


dim(ds_program_month)
summary(ds_program_month)
ds_program_month # Aggregated, so there's no PHI

rm(ds_possible_month, ds_program_month_visit, ds_case_load_program_month)

# d_unmatched <-
#   ds_program_month %>%
#   dplyr::filter(is.na(service_capacity_count)) %>%
#   dplyr::count(program_code, program_name)


ds_provider_week <- ds_lu_worker %>%
  tidyr::crossing(ds_possible_week) %>%
  # dplyr::left_join(ds_program_week_referral       , by=program_provider_week_keys) %>%
  # dplyr::left_join(ds_program_week_enroll         , by=program_provider_week_keys) %>%
  dplyr::left_join(ds_provider_week_visit           , by=program_provider_week_keys) %>%
  dplyr::left_join(ds_provider_week_conversion      , by=program_provider_week_keys) %>%
  dplyr::left_join(ds_case_load_provider_week       , by=program_provider_week_keys) %>%
  dplyr::left_join(ds_provider_week_visit_goal      , by=program_provider_week_keys) %>%
  dplyr::left_join(ds_provider_week_cum             , by=program_provider_week_keys) %>%
  dplyr::left_join(ds_provider_week_retention_shifted , by=program_provider_week_keys) %>%
  dplyr::mutate(
    # referral_accepted_count     = dplyr::coalesce(referral_accepted_count   , 0L),
    # enroll_count                = dplyr::coalesce(enroll_count              , 0L),
    visit_completed_count         = dplyr::coalesce(visit_completed_count     , 0L),
    visit_scheduled_count         = dplyr::coalesce(visit_scheduled_count     , 0L),
    case_load_count               = dplyr::coalesce(case_load_count           , 0L),

    brain_video_shown_count                   = dplyr::coalesce(brain_video_shown_count                   , 0L),
    brain_video_chance_count                  = dplyr::coalesce(brain_video_chance_count                  , 0L),
    convert_referral_to_enroll_count          = dplyr::coalesce(convert_referral_to_enroll_count          , 0L),
    convert_referral_to_enroll_denominator    = dplyr::coalesce(convert_referral_to_enroll_denominator    , 0L),
    convert_enroll_to_visit_2_count           = dplyr::coalesce(convert_enroll_to_visit_2_count           , 0L),
    convert_enroll_to_visit_2_denominator     = dplyr::coalesce(convert_enroll_to_visit_2_denominator     , 0L),
    visit_first_is_alternative_count          = dplyr::coalesce(visit_first_is_alternative_count          , 0L),
    scheduled_unique_client_count             = dplyr::coalesce(scheduled_unique_client_count             , 0L),

    retention_month_01_denominator            = dplyr::coalesce(retention_month_01_denominator            , 0L),
    retention_month_02_denominator            = dplyr::coalesce(retention_month_02_denominator            , 0L),
    retention_month_03_denominator            = dplyr::coalesce(retention_month_03_denominator            , 0L),
    retention_month_06_denominator            = dplyr::coalesce(retention_month_06_denominator            , 0L),
    retention_month_09_denominator            = dplyr::coalesce(retention_month_09_denominator            , 0L),
    retention_month_12_denominator            = dplyr::coalesce(retention_month_12_denominator            , 0L),
    retention_month_15_denominator            = dplyr::coalesce(retention_month_15_denominator            , 0L),
    retention_month_18_denominator            = dplyr::coalesce(retention_month_18_denominator            , 0L),
    retention_month_21_denominator            = dplyr::coalesce(retention_month_21_denominator            , 0L),
    retention_month_24_denominator            = dplyr::coalesce(retention_month_24_denominator            , 0L)
  ) %>%
  # There's no integration w/ Cherokee here (like for program_month above), b/c we don't have week-level data from them.
  dplyr::left_join(
    ds_lu_program %>%
      dplyr::select(program_code, miechv_3),
    by = "program_code"
  )

# ds_provider_week %>%
#   dplyr::count(worker_name, week, program_code) %>%
#   dplyr::filter(n>1)

dim(ds_provider_week)
summary(ds_provider_week)
ds_provider_week # Aggregated, so there's no PHI

rm(program_month_keys, program_provider_week_keys)
rm(ds_lu_worker, ds_possible_week, ds_provider_week_visit, ds_case_load_provider_week)

# ds_provider_week %>%
#   dplyr::count(worker_name, program_code)


# ---- filter-miechv3-programs -------------------------------------------------
ds_program_month %>%
  dplyr::filter(is.na(miechv_3)) %>%
  dplyr::count(program_code, program_name, miechv_3) %>%
  print(n=70)

ds_program_month <- ds_program_month %>%
  dplyr::filter(miechv_3)

ds_provider_week <- ds_provider_week %>%
  dplyr::filter(miechv_3)


# ---- order-columns -----------------------------------------------------------
ds_program_month <- ds_program_month %>%
  dplyr::select(
    program_code,
    # worker_name,
    # month,
    dplyr::everything(), #This optionally picks up 'month', 'week', and 'worker_name

    model_name,
    model_id,
    program_name,
    miechv_3,

    # referral_accepted_count,
    # enroll_count,
    visit_completed_count,
    visit_scheduled_count,
    visit_completed_per_scheduled,
    case_load_count,
    scheduled_unique_client_count
  )

ds_provider_week <- ds_provider_week %>%
  dplyr::select(
    program_code,
    # worker_name,
    # month,
    dplyr::everything(), #This optionally picks up 'month', 'week', and 'worker_name

    model_name,
    model_id,
    program_name,
    miechv_3,

    # referral_accepted_count,
    # enroll_count,
    visit_completed_count,
    visit_scheduled_count,
    visit_completed_per_scheduled,
    case_load_count,
    scheduled_unique_client_count
  )

# ---- aggregate-to-model-and-to-program ---------------------------------------
ds_model <- ds_program_month %>%
  dplyr::group_by(model_id, model_name) %>%
  dplyr::summarize(
    reached_goal_proportion       = weighted.mean(reached_goal_proportion      , w=case_load_count, na.rm=T),
    # visit_count_past_4_weeks_mean = weighted.mean(visit_count_past_4_weeks_mean, w=case_load_count, na.rm=T),

    convert_referral_to_enroll_proportion     = sum(convert_referral_to_enroll_count, na.rm=T) / sum(convert_referral_to_enroll_denominator, na.rm=T),
    convert_enroll_to_visit_2_proportion      = sum(convert_enroll_to_visit_2_count , na.rm=T) / sum(convert_enroll_to_visit_2_denominator , na.rm=T),
    convert_referral_to_enroll_denominator    = sum(convert_referral_to_enroll_denominator, na.rm=T),
    convert_enroll_to_visit_2_denominator     = sum(convert_enroll_to_visit_2_denominator , na.rm=T),
    visit_first_is_alternative_count          = sum(visit_first_is_alternative_count, na.rm=T),

    case_load_count               = sum(case_load_count, na.rm=T),
    scheduled_unique_client_count = sum(scheduled_unique_client_count, na.rm=T),

    service_capacity_count        = sum(service_capacity_count, na.rm=T),
    # referral_accepted_count     = sum(referral_accepted_count),
    # enroll_count                = sum(enroll_count),
    visit_completed_count         = sum(visit_completed_count),
    visit_scheduled_count         = sum(visit_scheduled_count),
    visit_completed_per_scheduled = visit_completed_count / visit_scheduled_count,

    case_load_monthly             = case_load_count           / dplyr::n_distinct(month), # The numerator is calculated above
    scheduled_unique_client_monthly=scheduled_unique_client_count/ dplyr::n_distinct(month), # The numerator is calculated above
    service_capacity_monthly      = service_capacity_count    / dplyr::n_distinct(month), # The numerator is calculated above
    # referral_accepted_monthly   = referral_accepted_count   / dplyr::n_distinct(month), # The numerator is calculated above
    # enroll_monthly              = enroll_count              / dplyr::n_distinct(month), # The numerator is calculated above
    visit_completed_monthly       = visit_completed_count     / dplyr::n_distinct(month), # The numerator is calculated above
    visit_scheduled_monthly       = visit_scheduled_count     / dplyr::n_distinct(month), # The numerator is calculated above


    brain_video_shown_proportion  = sum(brain_video_shown_count, na.rm=T) / sum(brain_video_chance_count, na.rm=T),
    brain_video_shown_count       = sum(brain_video_shown_count, na.rm=T),
    brain_video_chance_count      = sum(brain_video_chance_count, na.rm=T),

    retention_month_01_mean       = weighted.mean(retention_month_01_mean   , w=retention_month_01_denominator),
    retention_month_02_mean       = weighted.mean(retention_month_02_mean   , w=retention_month_02_denominator),
    retention_month_03_mean       = weighted.mean(retention_month_03_mean   , w=retention_month_03_denominator),
    retention_month_06_mean       = weighted.mean(retention_month_06_mean   , w=retention_month_06_denominator),
    retention_month_09_mean       = weighted.mean(retention_month_09_mean   , w=retention_month_09_denominator),
    retention_month_12_mean       = weighted.mean(retention_month_12_mean   , w=retention_month_12_denominator),
    retention_month_15_mean       = weighted.mean(retention_month_15_mean   , w=retention_month_15_denominator),
    retention_month_18_mean       = weighted.mean(retention_month_18_mean   , w=retention_month_18_denominator),
    retention_month_21_mean       = weighted.mean(retention_month_21_mean   , w=retention_month_21_denominator),
    retention_month_24_mean       = weighted.mean(retention_month_24_mean   , w=retention_month_24_denominator),

    retention_month_01_denominator    = sum(retention_month_01_denominator      , na.rm=T),
    retention_month_02_denominator    = sum(retention_month_02_denominator      , na.rm=T),
    retention_month_03_denominator    = sum(retention_month_03_denominator      , na.rm=T),
    retention_month_06_denominator    = sum(retention_month_06_denominator      , na.rm=T),
    retention_month_09_denominator    = sum(retention_month_09_denominator      , na.rm=T),
    retention_month_12_denominator    = sum(retention_month_12_denominator      , na.rm=T),
    retention_month_15_denominator    = sum(retention_month_15_denominator      , na.rm=T),
    retention_month_18_denominator    = sum(retention_month_18_denominator      , na.rm=T),
    retention_month_21_denominator    = sum(retention_month_21_denominator      , na.rm=T),
    retention_month_24_denominator    = sum(retention_month_24_denominator      , na.rm=T),

    case_capacity_proportion          = case_load_count / service_capacity_count
  ) %>%
  dplyr::ungroup()

ds_program <- ds_program_month %>%
  dplyr::group_by(program_code, model_id, model_name, program_name, miechv_3) %>%
  dplyr::summarize(
    reached_goal_proportion       = weighted.mean(reached_goal_proportion      , w=case_load_count, na.rm=T),
    # visit_count_past_4_weeks_mean = weighted.mean(visit_count_past_4_weeks_mean, w=case_load_count, na.rm=T),

    convert_referral_to_enroll_proportion     = sum(convert_referral_to_enroll_count, na.rm=T) / sum(convert_referral_to_enroll_denominator, na.rm=T),
    convert_enroll_to_visit_2_proportion      = sum(convert_enroll_to_visit_2_count , na.rm=T) / sum(convert_enroll_to_visit_2_denominator , na.rm=T),
    convert_referral_to_enroll_denominator    = sum(convert_referral_to_enroll_denominator, na.rm=T),
    convert_enroll_to_visit_2_denominator     = sum(convert_enroll_to_visit_2_denominator , na.rm=T),
    visit_first_is_alternative_count          = sum(visit_first_is_alternative_count, na.rm=T),

    case_load_count               = sum(case_load_count, na.rm=T),
    scheduled_unique_client_count = sum(scheduled_unique_client_count, na.rm=T),
    service_capacity_count        = sum(service_capacity_count, na.rm=T),
    # referral_accepted_count     = sum(referral_accepted_count),
    # enroll_count                = sum(enroll_count),
    visit_completed_count         = sum(visit_completed_count),
    visit_scheduled_count         = sum(visit_scheduled_count),
    visit_completed_per_scheduled = visit_completed_count / visit_scheduled_count,

    brain_video_shown_proportion  = sum(brain_video_shown_count, na.rm=T) / sum(brain_video_chance_count, na.rm=T),
    brain_video_shown_count       = sum(brain_video_shown_count, na.rm=T),
    brain_video_chance_count      = sum(brain_video_chance_count, na.rm=T),

    case_load_monthly             = case_load_count           / dplyr::n_distinct(month),
    scheduled_unique_client_monthly=scheduled_unique_client_count/ dplyr::n_distinct(month), # The numerator is calculated above
    service_capacity_monthly      = service_capacity_count    / dplyr::n_distinct(month),
    # referral_accepted_monthly   = referral_accepted_count   / dplyr::n_distinct(month),
    # enroll_monthly              = enroll_count              / dplyr::n_distinct(month),
    visit_completed_monthly       = visit_completed_count     / dplyr::n_distinct(month),
    visit_scheduled_monthly       = visit_scheduled_count     / dplyr::n_distinct(month),

    retention_month_01_mean       = weighted.mean(retention_month_01_mean   , w=retention_month_01_denominator),
    retention_month_02_mean       = weighted.mean(retention_month_02_mean   , w=retention_month_02_denominator),
    retention_month_03_mean       = weighted.mean(retention_month_03_mean   , w=retention_month_03_denominator),
    retention_month_06_mean       = weighted.mean(retention_month_06_mean   , w=retention_month_06_denominator),
    retention_month_09_mean       = weighted.mean(retention_month_09_mean   , w=retention_month_09_denominator),
    retention_month_12_mean       = weighted.mean(retention_month_12_mean   , w=retention_month_12_denominator),
    retention_month_15_mean       = weighted.mean(retention_month_15_mean   , w=retention_month_15_denominator),
    retention_month_18_mean       = weighted.mean(retention_month_18_mean   , w=retention_month_18_denominator),
    retention_month_21_mean       = weighted.mean(retention_month_21_mean   , w=retention_month_21_denominator),
    retention_month_24_mean       = weighted.mean(retention_month_24_mean   , w=retention_month_24_denominator),

    retention_month_01_denominator    = sum(retention_month_01_denominator      , na.rm=T),
    retention_month_02_denominator    = sum(retention_month_02_denominator      , na.rm=T),
    retention_month_03_denominator    = sum(retention_month_03_denominator      , na.rm=T),
    retention_month_06_denominator    = sum(retention_month_06_denominator      , na.rm=T),
    retention_month_09_denominator    = sum(retention_month_09_denominator      , na.rm=T),
    retention_month_12_denominator    = sum(retention_month_12_denominator      , na.rm=T),
    retention_month_15_denominator    = sum(retention_month_15_denominator      , na.rm=T),
    retention_month_18_denominator    = sum(retention_month_18_denominator      , na.rm=T),
    retention_month_21_denominator    = sum(retention_month_21_denominator      , na.rm=T),
    retention_month_24_denominator    = sum(retention_month_24_denominator      , na.rm=T),

    case_capacity_proportion          = case_load_count / service_capacity_count
  ) %>%
  dplyr::ungroup()


# ---- inspect-program-month -----------------------------------------------------------------
cat(
  "Unique programs    : ", scales::comma(dplyr::n_distinct(ds_program_month$program_code)), "\n",
  "Unique months      : ", scales::comma(dplyr::n_distinct(ds_program_month$month       )), "\n",
  "Date range         : ", strftime(range(ds_program_month$month), "%Y-%m-%d  "), "\n",
  sep=""
)
ds_program_month %>%
  dplyr::count(model_name) %>%
  dplyr::mutate(n = scales::comma(n)) %>%
  tidyr::spread(model_name, n)

ds_program_month %>%
  # dplyr::filter(visit_all_completed_count > 0L) %>%
  # purrr::map(., ~mean(is.na(.)) ) %>%
  purrr::map(., ~mean(is.na(.) | as.character(.)=="Unknown")) %>%
  purrr::map(., ~round(., 3)) %>%
  tibble::as_tibble() %>%
  t()

# ---- inspect-provider-week -----------------------------------------------------------------
cat(
  "Unique programs    : ", scales::comma(dplyr::n_distinct(ds_provider_week$program_code)), "\n",
  "Unique providers   : ", scales::comma(dplyr::n_distinct(ds_provider_week$worker_name )), "\n",
  "Unique months      : ", scales::comma(dplyr::n_distinct(ds_provider_week$week        )), "\n",
  "Date range         : ", strftime(range(ds_provider_week$week), "%Y-%m-%d  "), "\n",
  sep=""
)
ds_provider_week %>%
  dplyr::count(model_name) %>%
  dplyr::mutate(n = scales::comma(n)) %>%
  tidyr::spread(model_name, n)

ds_provider_week %>%
  # dplyr::filter(visit_all_completed_count > 0L) %>%
  # purrr::map(., ~mean(is.na(.)) ) %>%
  purrr::map(., ~mean(is.na(.) | as.character(.)=="Unknown")) %>%
  purrr::map(., ~round(., 3)) %>%
  tibble::as_tibble() %>%
  t()

# ---- verify-values-model -----------------------------------------------------------
checkmate::assert_integer(  ds_model$model_id                          , lower=    1       , upper=    5               , any.missing=F)
checkmate::assert_factor(   ds_model$model_name                                                                        , any.missing=F)

checkmate::assert_numeric(  ds_model$reached_goal_proportion           , lower=    0       , upper=1               , any.missing=F)
# checkmate::assert_numeric(  ds_model$visit_count_past_4_weeks_mean     , lower=    0       , upper=6               , any.missing=F)

checkmate::assert_integer(  ds_model$case_load_count                   , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_model$scheduled_unique_client_count     , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_model$service_capacity_count            , lower=    0       , upper=900000              , any.missing=F)
# checkmate::assert_integer(ds_model$referral_accepted_count           , lower=    0       , upper= 1200               , any.missing=F)
# checkmate::assert_integer(ds_model$enroll_count                      , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_integer(  ds_model$visit_completed_count             , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_model$visit_scheduled_count             , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_model$visit_completed_per_scheduled     , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_integer(  ds_model$brain_video_shown_count           , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_model$brain_video_chance_count          , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_model$brain_video_shown_proportion      , lower=    0       , upper=1                   , any.missing=F)

checkmate::assert_numeric(  ds_model$case_load_monthly                 , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_model$service_capacity_monthly          , lower=    0       , upper=900000              , any.missing=F)
# checkmate::assert_numeric(ds_model$referral_accepted_monthly         , lower=    0       , upper= 1200               , any.missing=F)
# checkmate::assert_numeric(ds_model$enroll_monthly                    , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_numeric(  ds_model$visit_completed_monthly           , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_model$visit_scheduled_monthly           , lower=    0       , upper=900000              , any.missing=F)

checkmate::assert_numeric(  ds_model$retention_month_01_mean           , lower=    0       , upper=1                   , any.missing=T)
checkmate::assert_numeric(  ds_model$retention_month_03_mean           , lower=    0       , upper=1                   , any.missing=T)
checkmate::assert_numeric(  ds_model$retention_month_01_denominator    , lower=    0       , upper=10000               , any.missing=T)
checkmate::assert_numeric(  ds_model$retention_month_03_denominator    , lower=    0       , upper=10000               , any.missing=T)

checkmate::assert_numeric(  ds_model$case_capacity_proportion          , lower=    0       , upper= 300                , any.missing=T)

# ---- verify-values-program -----------------------------------------------------------
checkmate::assert_integer(  ds_program$program_code                      , lower=    1       , upper=  899               , any.missing=F)
checkmate::assert_character(ds_program$program_name                      , min.chars = 7                                 , any.missing=F)
checkmate::assert_integer(  ds_program$model_id                          , lower=    1       , upper=    5               , any.missing=F)
checkmate::assert_factor(   ds_program$model_name                                                                        , any.missing=F)
checkmate::assert_logical(  ds_program$miechv_3                           , any.missing=F)

# checkmate::assert_numeric(  ds_program$reached_goal_proportion           , lower=    0       , upper=1               , any.missing=F)
# checkmate::assert_numeric(  ds_program$visit_count_past_4_weeks_mean     , lower=    0       , upper=6               , any.missing=F)

checkmate::assert_integer(  ds_program$case_load_count                   , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_program$scheduled_unique_client_count     , lower=    0       , upper=900000              , any.missing=T) # Cherokee doesn't have this.
checkmate::assert_integer(  ds_program$service_capacity_count            , lower=    0       , upper=900000              , any.missing=F)
# checkmate::assert_integer(ds_program$referral_accepted_count           , lower=    0       , upper= 1200               , any.missing=F)
# checkmate::assert_integer(ds_program$enroll_count                      , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_integer(  ds_program$visit_completed_count             , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_program$visit_scheduled_count             , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_program$visit_completed_per_scheduled     , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_integer(  ds_program$brain_video_shown_count           , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_integer(  ds_program$brain_video_chance_count          , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_program$brain_video_shown_proportion      , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_numeric(  ds_program$case_load_monthly                 , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_program$service_capacity_monthly          , lower=    0       , upper=900000              , any.missing=F)
# checkmate::assert_numeric(ds_program$referral_accepted_monthly         , lower=    0       , upper= 1200               , any.missing=F)
# checkmate::assert_numeric(ds_program$enroll_monthly                    , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_numeric(  ds_program$visit_completed_monthly           , lower=    0       , upper=900000              , any.missing=F)
checkmate::assert_numeric(  ds_program$visit_scheduled_monthly           , lower=    0       , upper=900000              , any.missing=F)

checkmate::assert_numeric(  ds_program$case_capacity_proportion[ds_program$miechv_3]          , lower=    0       , upper= 300               , any.missing=T)


# ---- verify-values-program-month -----------------------------------------------------------
checkmate::assert_date(     ds_program_month$month                              , lower=range_visit[1]  , upper=range_visit[2]  , any.missing=F)
checkmate::assert_integer(  ds_program_month$program_code                       , lower=    1       , upper=  899               , any.missing=F)
checkmate::assert_character(ds_program_month$program_name                       , min.chars = 7                                 , any.missing=F)
checkmate::assert_factor(   ds_program_month$model_name                                                                         , any.missing=F)
checkmate::assert_integer(  ds_program_month$model_id                           , lower=    1       , upper=    5               , any.missing=F)
checkmate::assert_logical(  ds_program_month$miechv_3                           , any.missing=F)

checkmate::assert_numeric(  ds_program_month$reached_goal_proportion           , lower=    0       , upper=1               , any.missing=T)
# checkmate::assert_numeric(  ds_program_month$visit_count_past_4_weeks_mean     , lower=    0       , upper=6               , any.missing=T)

checkmate::assert_integer(  ds_program_month$case_load_count                    , lower=    0       , upper=  500               , any.missing=F)
checkmate::assert_integer(  ds_program_month$scheduled_unique_client_count                    , lower=    0       , upper=  500 , any.missing=T)# Cherokee doesn't have this.
# checkmate::assert_integer(  ds_program_month$service_capacity_count             , lower=    0       , upper=  500               , any.missing=F)
# checkmate::assert_integer(ds_program_month$referral_accepted_count            , lower=    0       , upper= 1200               , any.missing=F)
# checkmate::assert_integer(ds_program_month$enroll_count                       , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_integer(  ds_program_month$visit_completed_count              , lower=    0       , upper= 1800               , any.missing=F)
checkmate::assert_integer(  ds_program_month$visit_scheduled_count              , lower=    0       , upper= 1800               , any.missing=F)
checkmate::assert_numeric(  ds_program_month$visit_completed_per_scheduled      , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_integer(  ds_program_month$brain_video_shown_count            , lower=    0       , upper=900000              , any.missing=T)
checkmate::assert_integer(  ds_program_month$brain_video_chance_count           , lower=    0       , upper=900000              , any.missing=T)
checkmate::assert_numeric(  ds_program_month$brain_video_shown_proportion       , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_numeric(  ds_program_month$case_capacity_proportion           , lower=    0       , upper= 300                , any.missing=T)

# a <- paste(ds_program_month$program_code, ds_program_month$month)
# a[duplicated(a)]
checkmate::assert_character(paste(ds_program_month$program_code, ds_program_month$month)      , pattern="^\\d{3} \\d{4}-\\d{2}-\\d{2}"  , unique=T , any.missing=F)

# ---- verify-values-provider-week -----------------------------------------------------------
table( ds_provider_week$program_code[is.na(ds_provider_week$program_name)])
checkmate::assert_date(     ds_provider_week$week                               , lower=range_visit[1]  , upper=range_visit[2]  , any.missing=F)
checkmate::assert_character(ds_provider_week$worker_name                        , min.chars = 7                                 , any.missing=F)
checkmate::assert_integer(  ds_provider_week$program_code                       , lower=    1       , upper=  899               , any.missing=F)
checkmate::assert_character(ds_provider_week$program_name                       , min.chars = 7                                 , any.missing=F)
checkmate::assert_integer(  ds_provider_week$model_id                           , lower=    1       , upper=    5               , any.missing=F)
checkmate::assert_factor(   ds_provider_week$model_name                                                                         , any.missing=F)
checkmate::assert_logical(  ds_provider_week$miechv_3                           , any.missing=F)

checkmate::assert_numeric(  ds_provider_week$reached_goal_proportion           , lower=    0       , upper=1               , any.missing=T)
checkmate::assert_numeric(  ds_provider_week$visit_count_past_4_weeks_mean     , lower=    0       , upper=16              , any.missing=T)

# plot(ds_provider_week$client_count, ds_provider_week$case_load_count); abline(a=0, b=1)
checkmate::assert_integer(  ds_provider_week$case_load_count                    , lower=    0       , upper=  100               , any.missing=F)
checkmate::assert_integer(  ds_provider_week$scheduled_unique_client_count                    , lower=    0       , upper=  100               , any.missing=F)

# checkmate::assert_integer(ds_provider_week$referral_accepted_count            , lower=    0       , upper= 1200               , any.missing=F)
# checkmate::assert_integer(ds_provider_week$enroll_count                       , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_integer(  ds_provider_week$visit_completed_count              , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_integer(  ds_provider_week$visit_scheduled_count              , lower=    0       , upper= 1200               , any.missing=F)
checkmate::assert_numeric(  ds_provider_week$visit_completed_per_scheduled      , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_integer(  ds_provider_week$brain_video_shown_count            , lower=    0       , upper=900000              , any.missing=T)
checkmate::assert_integer(  ds_provider_week$brain_video_chance_count           , lower=    0       , upper=900000              , any.missing=T)
checkmate::assert_numeric(  ds_provider_week$brain_video_shown_proportion       , lower=    0       , upper=1                   , any.missing=T)

checkmate::assert_character(paste(ds_provider_week$worker_name, ds_provider_week$week, ds_provider_week$program_code), unique=T , any.missing=F)

# ---- save-to-disk ------------------------------------------------------------
readr::write_rds(ds_model             , path_out_model          , compress="gz")
readr::write_rds(ds_program           , path_out_program        , compress="gz")
readr::write_rds(ds_program_month     , path_out_program_month  , compress="gz")
readr::write_rds(ds_provider_week     , path_out_provider_week  , compress="gz")

