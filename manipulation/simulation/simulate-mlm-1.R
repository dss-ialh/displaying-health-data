# knitr::stitch_rmd(script="./manipulation/simulation/simulate-mlm-1.R", output="./stitched-output/manipulation/simulation/simulate-mlm-1.md") # dir.create("./stitched-output/manipulation/simulation", recursive=T)
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

# ---- load-sources ------------------------------------------------------------
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.

# ---- load-packages -----------------------------------------------------------
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly=TRUE)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("rlang"        ) # Language constucts, like quosures
requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("DBI"          ) # Database-agnostic interface
requireNamespace("RSQLite"      ) # Lightweight database for non-PHI data.
# requireNamespace("RODBC"      ) # For communicating with SQL Server over a locally-configured DSN.  Uncomment if you use 'upload-to-db' chunk.
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")

# ---- declare-globals ---------------------------------------------------------
# Constant values that won't change.
config                         <- config::get()
set.seed(453)
figure_path <- 'stitched-output/manipulation/simulation/simulate-mlm-1/'


subject_count       <- 20
wave_count          <- 10
loadings_factor_1   <- c(.4, .5, .6)
sigma_factor_1      <- c(.1, .2, .1)
possible_year_start <- 2000:2005


# ---- load-data ---------------------------------------------------------------

# ---- tweak-data --------------------------------------------------------------

# ---- generate ----------------------------------------------------------------
ds_subject <-
  tibble::tibble(
    subject_id      = factor(seq_len(subject_count)),
    year_start      = sample(possible_year_start, size=subject_count, replace=T),

    int_factor_1    = rnorm(n=subject_count, mean=10.0, sd=2.0),
    slope_factor_1  = rnorm(n=subject_count, mean= 0.05, sd=0.04)
  )
ds_subject

ds <-
  tidyr::crossing(
    subject_id      = ds_subject$subject_id,
    wave_id         = seq_len(wave_count)
  ) %>%
  dplyr::right_join(ds_subject, by="subject_id") %>%
  dplyr::mutate(
    year            = wave_id + year_start - 1L,
    cog_1           =
      (int_factor_1 * loadings_factor_1[1]) +
      slope_factor_1 * wave_id +
      rnorm(n=n(), mean=0, sd=sigma_factor_1[1]),
    cog_2           =
      (int_factor_1 * loadings_factor_1[2]) +
      slope_factor_1 * wave_id +
      rnorm(n=n(), mean=0, sd=sigma_factor_1[2]),
    cog_3           =
      (int_factor_1 * loadings_factor_1[3]) +
      slope_factor_1 * wave_id +
      rnorm(n=n(), mean=0, sd=sigma_factor_1[3])
  ) %>%
  dplyr::select(-year_start)

ds


# ---- elongate --------------------------------------------------------------------
ds_long <-
  ds %>%
  dplyr::select(
    subject_id,
    wave_id,
    year,
    cog_1,
    cog_2,
    cog_3
  ) %>%
  tidyr::gather(key=manifest, value=value, -subject_id, -wave_id, -year)


# ---- inspect, fig.width=10, fig.height=6, fig.path=figure_path -----------------------------------------------------------------
library(ggplot2)

ggplot(ds_long, aes(x=year, y=value, color=subject_id)) +
  geom_line() +
  facet_wrap("manifest", ncol=3) +
  theme_minimal() +
  theme(legend.position="none")


ggplot(ds, aes(x=year, y=cog_1, color=subject_id)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position="none")

# ---- specify-columns-to-upload -----------------------------------------------
# dput(colnames(ds)) # Print colnames for line below.
columns_to_write <- c(
  "subject_id", "wave_id",
  "year",
  "int_factor_1", "slope_factor_1",
  "cog_1", "cog_2", "cog_3"
)
ds_slim <-
  ds %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(!!columns_to_write)
ds_slim

rm(columns_to_write)

# ---- save-to-disk ------------------------------------------------------------
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
readr::write_csv(ds_slim, config$path_mlm_1)
# readr::write_rds(ds_slim, path_out_unified, compress="gz") # Save as a compressed R-binary file if it's large or has a lot of factors.

