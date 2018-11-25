



This report was automatically generated with the R package **knitr**
(version 1.20).


```r
# knitr::stitch_rmd(script="./manipulation/simulation/simulate-mlm-1.R", output="./stitched-output/manipulation/simulation/simulate-mlm-1.md") # dir.create("./stitched-output/manipulation/simulation", recursive=T)
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
```

```r
# Call `base::source()` on any repo file that defines functions needed below.  Ideally, no real operations are performed.
```

```r
# Attach these package(s) so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr            , quietly=TRUE)

# Verify these packages are available on the machine, but their functions need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
requireNamespace("readr"        )
```

```
## Loading required namespace: readr
```

```r
requireNamespace("tidyr"        )
```

```
## Loading required namespace: tidyr
```

```r
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
```

```
## Loading required namespace: dplyr
```

```r
requireNamespace("rlang"        ) # Language constucts, like quosures
requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
```

```
## Loading required namespace: testit
```

```r
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
```

```
## Loading required namespace: checkmate
```

```r
requireNamespace("DBI"          ) # Database-agnostic interface
```

```
## Loading required namespace: DBI
```

```r
requireNamespace("RSQLite"      ) # Lightweight database for non-PHI data.
```

```
## Loading required namespace: RSQLite
```

```r
# requireNamespace("RODBC"      ) # For communicating with SQL Server over a locally-configured DSN.  Uncomment if you use 'upload-to-db' chunk.
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")
```

```
## Loading required namespace: OuhscMunge
```

```r
# Constant values that won't change.
config                         <- config::get()
set.seed(453)
figure_path <- 'stitched-output/manipulation/simulation/simulate-mlm-1/'

subject_count       <- 20
wave_count          <- 10

possible_year_start <- 2000:2005
possible_age_start  <- 55:75
possible_county_id  <- c(51L, 55L, 72L)
possible_county_index  <- seq_along(possible_county_id)
possible_gender_id     <- c(1L, 2L, 255L)
possible_race          <- c(
  "American Indian/Alaska Native",
  "Asian",
  "Native Hawaiian or Other Pacific Islander",
  "Black or African American",
  "White",
  "More than One Race",
  "Unknown or Not Reported"
)
possible_ethnicity <- c(
  "Not Hispanic or Latino",
  "Hispanic or Latino",
  "Unknown/Not Reported Ethnicity"
)

int_county          <- c(2, 2.1, 4)
slope_county        <- c(-.04, -.06, -.2)

cor_factor_1_vs_2   <- c(.3, .005)          # Int & slope
loadings_factor_1   <- c(.4, .5, .6)
loadings_factor_2   <- c(.3, .4, .1)
sigma_factor_1      <- c(.1, .2, .1)
sigma_factor_2      <- c(.2, .3, .5)
```



```r
ds_subject <-
  tibble::tibble(
    subject_id      = factor(1000 + seq_len(subject_count)),
    year_start      = sample(possible_year_start, size=subject_count, replace=T),
    age_start       = sample(possible_age_start , size=subject_count, replace=T),
    county_index    = sample(possible_county_index , size=subject_count, replace=T),
    county_id       = possible_county_id[county_index],

    gender_id       = sample(possible_gender_id , size=subject_count, replace=T, prob=c(.4, .5, .1)),
    race            = sample(possible_race      , size=subject_count, replace=T),
    ethnicity       = sample(possible_ethnicity , size=subject_count, replace=T)

  ) %>%
  dplyr::mutate(
    int_factor_1    = int_county[county_index]   + rnorm(n=subject_count, mean=10.0, sd=2.0),
    slope_factor_1  = slope_county[county_index] + rnorm(n=subject_count, mean= 0.05, sd=0.04),

    int_factor_2    = rnorm(n=subject_count, mean=5.0, sd=0.8) + (cor_factor_1_vs_2[1] * int_factor_1),
    slope_factor_2  = rnorm(n=subject_count, mean= 0.03, sd=0.02) + (cor_factor_1_vs_2[2] * int_factor_1)
  )
ds_subject
```

```
## # A tibble: 20 x 12
##    subject_id year_start age_start county_index county_id gender_id race 
##    <fct>           <int>     <int>        <int>     <int>     <int> <chr>
##  1 1001             2000        67            1        51         2 Nati…
##  2 1002             2000        66            2        55         2 Unkn…
##  3 1003             2004        73            3        72         2 Blac…
##  4 1004             2001        70            3        72       255 Amer…
##  5 1005             2005        55            3        72         1 Unkn…
##  6 1006             2003        74            1        51         1 Amer…
##  7 1007             2005        62            1        51       255 White
##  8 1008             2000        55            2        55       255 Unkn…
##  9 1009             2002        73            1        51         1 More…
## 10 1010             2004        69            1        51         1 Amer…
## 11 1011             2000        66            1        51         2 Amer…
## 12 1012             2001        66            3        72       255 More…
## 13 1013             2005        72            1        51         2 Asian
## 14 1014             2003        71            1        51         1 Unkn…
## 15 1015             2004        66            3        72         2 Amer…
## 16 1016             2000        61            2        55         2 Asian
## 17 1017             2004        59            3        72         1 Nati…
## 18 1018             2001        74            1        51         1 White
## 19 1019             2001        70            2        55         1 Asian
## 20 1020             2004        73            2        55         2 More…
## # ... with 5 more variables: ethnicity <chr>, int_factor_1 <dbl>,
## #   slope_factor_1 <dbl>, int_factor_2 <dbl>, slope_factor_2 <dbl>
```

```r
ds <-
  tidyr::crossing(
    subject_id      = ds_subject$subject_id,
    wave_id         = seq_len(wave_count)
  ) %>%
  dplyr::right_join(ds_subject, by="subject_id") %>%
  dplyr::mutate(
    year            = wave_id + year_start - 1L,
    age             = wave_id + age_start  - 1L,
  ) %>%
  dplyr::mutate( # Generate cognitive manifest variables (ie, from factor 1)
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
  dplyr::mutate( # Generate physical manifest variables (ie, from factor 2)
    phys_1           =
      (int_factor_2 * loadings_factor_2[1]) +
      slope_factor_2 * wave_id +
      rnorm(n=n(), mean=0, sd=sigma_factor_2[1]),
    phys_2           =
      (int_factor_2 * loadings_factor_2[2]) +
      slope_factor_2 * wave_id +
      rnorm(n=n(), mean=0, sd=sigma_factor_2[2]),
    phys_3           =
      (int_factor_2 * loadings_factor_2[3]) +
      slope_factor_2 * wave_id +
      rnorm(n=n(), mean=0, sd=sigma_factor_2[3])
  ) %>%
  dplyr::mutate( # Keep tha manifest variables positive (which will throw off the correlations)
    cog_1   = pmax(0, cog_1),
    cog_2   = pmax(0, cog_2),
    cog_3   = pmax(0, cog_3),
    phys_1  = pmax(0, phys_1),
    phys_2  = pmax(0, phys_2),
    phys_3  = pmax(0, phys_3)
  ) %>%
  dplyr::mutate( # Don't simulate unrealistically precise manfiest variables
    int_factor_1    = round(int_factor_1  , 3),
    slope_factor_1  = round(slope_factor_1, 3),
    int_factor_2    = round(int_factor_2  , 3),
    slope_factor_2  = round(slope_factor_2, 3),

    cog_1   = round(cog_1   , 1),
    cog_2   = round(cog_2   , 1),
    cog_3   = round(cog_3   , 1),
    phys_1  = round(phys_1  , 1),
    phys_2  = round(phys_2  , 1),
    phys_3  = round(phys_3  , 1)
  ) %>%
  dplyr::select(-year_start)

ds
```

```
## # A tibble: 200 x 20
##    subject_id wave_id age_start county_index county_id gender_id race 
##    <fct>        <int>     <int>        <int>     <int>     <int> <chr>
##  1 1001             1        67            1        51         2 Nati…
##  2 1001             2        67            1        51         2 Nati…
##  3 1001             3        67            1        51         2 Nati…
##  4 1001             4        67            1        51         2 Nati…
##  5 1001             5        67            1        51         2 Nati…
##  6 1001             6        67            1        51         2 Nati…
##  7 1001             7        67            1        51         2 Nati…
##  8 1001             8        67            1        51         2 Nati…
##  9 1001             9        67            1        51         2 Nati…
## 10 1001            10        67            1        51         2 Nati…
## # ... with 190 more rows, and 13 more variables: ethnicity <chr>,
## #   int_factor_1 <dbl>, slope_factor_1 <dbl>, int_factor_2 <dbl>,
## #   slope_factor_2 <dbl>, year <int>, age <int>, cog_1 <dbl>, cog_2 <dbl>,
## #   cog_3 <dbl>, phys_1 <dbl>, phys_2 <dbl>, phys_3 <dbl>
```

```r
ds_long <-
  ds %>%
  dplyr::select(
    subject_id,
    wave_id,
    year,
    age,
    county_id,
    cog_1,
    cog_2,
    cog_3,
    phys_1,
    phys_2,
    phys_3
  ) %>%
  tidyr::gather(
    key   = manifest,
    value = value, -subject_id, -wave_id, -year, -age, -county_id
  )
```

```r
library(ggplot2)

ggplot(ds_long, aes(x=wave_id, y=value, color=subject_id)) + #, ymin=0
  geom_line() +
  facet_wrap("manifest", ncol=3, scales="free_y") +
  theme_minimal() +
  theme(legend.position="none")
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-1.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
last_plot() + aes(x=year)
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-2.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
last_plot() + aes(x=age)
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-3.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
ggplot(ds, aes(x=year, y=cog_1, color=factor(county_id), group=subject_id)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position="top")
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-4.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
# OuhscMunge::verify_value_headstart(ds_subject)
checkmate::assert_factor(   ds_subject$subject_id     , any.missing=F                          , unique=T)
checkmate::assert_integer(  ds_subject$county_id      , any.missing=F , lower=51, upper=72     )
checkmate::assert_integer(  ds_subject$gender_id      , any.missing=F , lower=1, upper=255     )
checkmate::assert_character(ds_subject$race           , any.missing=F , pattern="^.{5,41}$"    )
checkmate::assert_character(ds_subject$ethnicity      , any.missing=F , pattern="^.{18,30}$"   )

# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_factor(  ds$subject_id        , any.missing=F                          )
checkmate::assert_integer( ds$wave_id           , any.missing=F , lower=1, upper=10      )
checkmate::assert_integer( ds$year              , any.missing=F , lower=2000, upper=2014 )
checkmate::assert_integer( ds$age               , any.missing=F , lower=55, upper=85     )
checkmate::assert_integer( ds$county_id         , any.missing=F , lower=1, upper=77      )

checkmate::assert_numeric( ds$int_factor_1      , any.missing=F , lower=4, upper=20      )
checkmate::assert_numeric( ds$slope_factor_1    , any.missing=F , lower=-1, upper=1      )
checkmate::assert_numeric( ds$int_factor_2      , any.missing=F , lower=6, upper=20      )
checkmate::assert_numeric( ds$slope_factor_2    , any.missing=F , lower=0, upper=1       )

checkmate::assert_numeric( ds$cog_1             , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$cog_2             , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$cog_3             , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$phys_1            , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$phys_2            , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$phys_3            , any.missing=F , lower=0, upper=20      )

subject_wave_combo   <- paste(ds$subject_id, ds$wave_id)
checkmate::assert_character(subject_wave_combo, pattern  ="^\\d{4} \\d{1,2}$"   , any.missing=F, unique=T)
```

```r
# dput(colnames(ds)) # Print colnames for line below.

ds_slim <-
  ds %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(
    !!c(
      "subject_id",
      "wave_id", "year", "age", "county_id",
      "int_factor_1", "slope_factor_1",
      "cog_1", "cog_2", "cog_3",
      "phys_1", "phys_2", "phys_3"
    )
  )
ds_slim
```

```
## # A tibble: 200 x 13
##    subject_id wave_id  year   age county_id int_factor_1 slope_factor_1
##    <fct>        <int> <int> <int>     <int>        <dbl>          <dbl>
##  1 1001             1  2000    67        51         8.90         -0.029
##  2 1001             2  2001    68        51         8.90         -0.029
##  3 1001             3  2002    69        51         8.90         -0.029
##  4 1001             4  2003    70        51         8.90         -0.029
##  5 1001             5  2004    71        51         8.90         -0.029
##  6 1001             6  2005    72        51         8.90         -0.029
##  7 1001             7  2006    73        51         8.90         -0.029
##  8 1001             8  2007    74        51         8.90         -0.029
##  9 1001             9  2008    75        51         8.90         -0.029
## 10 1001            10  2009    76        51         8.90         -0.029
## # ... with 190 more rows, and 6 more variables: cog_1 <dbl>, cog_2 <dbl>,
## #   cog_3 <dbl>, phys_1 <dbl>, phys_2 <dbl>, phys_3 <dbl>
```

```r
ds_slim_subject <-
  ds_subject %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(
    !!c(
      "subject_id",
      "county_id", # May intentionally exclude this from the outptu, to mimic what the ellis has to do sometimes.
      "gender_id",
      "race",
      "ethnicity"
    )
  )
ds_slim
```

```
## # A tibble: 200 x 13
##    subject_id wave_id  year   age county_id int_factor_1 slope_factor_1
##    <fct>        <int> <int> <int>     <int>        <dbl>          <dbl>
##  1 1001             1  2000    67        51         8.90         -0.029
##  2 1001             2  2001    68        51         8.90         -0.029
##  3 1001             3  2002    69        51         8.90         -0.029
##  4 1001             4  2003    70        51         8.90         -0.029
##  5 1001             5  2004    71        51         8.90         -0.029
##  6 1001             6  2005    72        51         8.90         -0.029
##  7 1001             7  2006    73        51         8.90         -0.029
##  8 1001             8  2007    74        51         8.90         -0.029
##  9 1001             9  2008    75        51         8.90         -0.029
## 10 1001            10  2009    76        51         8.90         -0.029
## # ... with 190 more rows, and 6 more variables: cog_1 <dbl>, cog_2 <dbl>,
## #   cog_3 <dbl>, phys_1 <dbl>, phys_2 <dbl>, phys_3 <dbl>
```

```r
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
readr::write_csv(ds_slim        , config$path_mlm_1_raw)
readr::write_csv(ds_slim_subject, config$path_subject_1_raw)
```

The R session information (including the OS info, R version and all
packages used):


```r
sessionInfo()
```

```
## R version 3.5.1 (2018-07-02)
## Platform: x86_64-pc-linux-gnu (64-bit)
## Running under: Ubuntu 18.04.1 LTS
## 
## Matrix products: default
## BLAS: /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.7.1
## LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.7.1
## 
## locale:
##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
## 
## attached base packages:
## [1] stats     graphics  grDevices utils     datasets  methods   base     
## 
## other attached packages:
## [1] ggplot2_3.1.0  bindrcpp_0.2.2 magrittr_1.5  
## 
## loaded via a namespace (and not attached):
##  [1] Rcpp_1.0.0            highr_0.7             plyr_1.8.4           
##  [4] pillar_1.3.0          compiler_3.5.1        bindr_0.1.1          
##  [7] tools_3.5.1           digest_0.6.18         packrat_0.5.0        
## [10] bit_1.1-14            gtable_0.2.0          evaluate_0.12        
## [13] RSQLite_2.1.1         memoise_1.1.0         tibble_1.4.2         
## [16] checkmate_1.8.9-9000  pkgconfig_2.0.2       rlang_0.3.0.1        
## [19] DBI_1.0.0             cli_1.0.1             yaml_2.2.0           
## [22] withr_2.1.2           dplyr_0.7.8           stringr_1.3.1        
## [25] knitr_1.20            hms_0.4.2.9001        grid_3.5.1           
## [28] bit64_0.9-7           tidyselect_0.2.5      glue_1.3.0           
## [31] OuhscMunge_0.1.9.9009 R6_2.3.0              fansi_0.4.0          
## [34] tidyr_0.8.2           readr_1.2.1           purrr_0.2.5          
## [37] blob_1.1.1            scales_1.0.0          backports_1.1.2      
## [40] assertthat_0.2.0      testit_0.8.1          colorspace_1.3-2     
## [43] labeling_0.3          config_0.3            utf8_1.1.4           
## [46] stringi_1.2.4         lazyeval_0.2.1        munsell_0.5.0        
## [49] crayon_1.3.4
```

```r
Sys.time()
```

```
## [1] "2018-11-25 10:58:23 PST"
```

