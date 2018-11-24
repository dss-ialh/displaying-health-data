



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
requireNamespace("tidyr"        )
requireNamespace("dplyr"        ) # Avoid attaching dplyr, b/c its function names conflict with a lot of packages (esp base, stats, and plyr).
requireNamespace("rlang"        ) # Language constucts, like quosures
requireNamespace("testit"       ) # For asserting conditions meet expected patterns/conditions.
requireNamespace("checkmate"    ) # For asserting conditions meet expected patterns/conditions. # remotes::install_github("mllg/checkmate")
requireNamespace("DBI"          ) # Database-agnostic interface
requireNamespace("RSQLite"      ) # Lightweight database for non-PHI data.
# requireNamespace("RODBC"      ) # For communicating with SQL Server over a locally-configured DSN.  Uncomment if you use 'upload-to-db' chunk.
requireNamespace("OuhscMunge"   ) # remotes::install_github(repo="OuhscBbmc/OuhscMunge")
```

```r
# Constant values that won't change.
config                         <- config::get()
set.seed(453)
figure_path <- 'stitched-output/manipulation/simulation/simulate-mlm-1/'

subject_count       <- 20
wave_count          <- 10
possible_year_start <- 2000:2005
possible_age_start  <- 70:76

cor_factor_1_vs_2   <- c(.3, .005)          # Int & slope
loadings_factor_1   <- c(.4, .5, .6)
loadings_factor_2   <- c(.3, .4, .1)
sigma_factor_1      <- c(.1, .2, .1)
sigma_factor_2      <- c(.2, .3, .5)
```



```r
ds_subject <-
  tibble::tibble(
    subject_id      = factor(seq_len(subject_count)),
    year_start      = sample(possible_year_start, size=subject_count, replace=T),
    age_start       = sample(possible_age_start , size=subject_count, replace=T)
  ) %>%
  dplyr::mutate(
    int_factor_1    = rnorm(n=subject_count, mean=10.0, sd=2.0),
    slope_factor_1  = rnorm(n=subject_count, mean= 0.05, sd=0.04),

    int_factor_2    = rnorm(n=subject_count, mean=5.0, sd=0.8) + (cor_factor_1_vs_2[1] * int_factor_1),
    slope_factor_2  = rnorm(n=subject_count, mean= 0.03, sd=0.02) + (cor_factor_1_vs_2[2] * int_factor_1)
  )
ds_subject
```

```
## # A tibble: 20 x 7
##    subject_id year_start age_start int_factor_1 slope_factor_1 int_factor_2
##    <fct>           <int>     <int>        <dbl>          <dbl>        <dbl>
##  1 1                2000        74         8.27        0.0282          6.24
##  2 2                2000        73        11.4         0.0514          7.31
##  3 3                2004        76        12.4         0.108           9.13
##  4 4                2001        75         6.10        0.0616          7.22
##  5 5                2005        70         7.40        0.0810          7.54
##  6 6                2003        76         6.85       -0.0462          6.59
##  7 7                2005        72         7.72        0.0257          6.50
##  8 8                2000        70        13.1        -0.00384         7.27
##  9 9                2002        76        13.4         0.0370          9.09
## 10 10               2004        74        10.1         0.0143          8.54
## 11 11               2000        73         8.61        0.0611          7.15
## 12 12               2001        73         7.80        0.0114          7.83
## 13 13               2005        75        11.0         0.0765          7.46
## 14 14               2003        75        15.4         0.0707         10.2 
## 15 15               2004        73        11.7         0.0990          9.31
## 16 16               2000        72         8.40       -0.0298          8.99
## 17 17               2004        71         7.76        0.0196          6.99
## 18 18               2001        76         7.45        0.0191          8.44
## 19 19               2001        75        12.5         0.141           8.26
## 20 20               2004        76        11.5         0.0683          8.05
## # ... with 1 more variable: slope_factor_2 <dbl>
```

```r
ds <-
  tidyr::crossing(
    subject_id      = ds_subject$subject_id,
    wave_id         = seq_len(wave_count)
  ) %>%
  dplyr::right_join(ds_subject, by="subject_id") %>%
  dplyr::arrange(subject_id, wave_id) %>%
  tibble::rowid_to_column("subject_wave_id") %>%
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
  dplyr::select(-year_start)

ds
```

```
## # A tibble: 200 x 16
##    subject_wave_id subject_id wave_id age_start int_factor_1 slope_factor_1
##              <int> <fct>        <int>     <int>        <dbl>          <dbl>
##  1               1 1                1        74         8.27         0.0282
##  2               2 1                2        74         8.27         0.0282
##  3               3 1                3        74         8.27         0.0282
##  4               4 1                4        74         8.27         0.0282
##  5               5 1                5        74         8.27         0.0282
##  6               6 1                6        74         8.27         0.0282
##  7               7 1                7        74         8.27         0.0282
##  8               8 1                8        74         8.27         0.0282
##  9               9 1                9        74         8.27         0.0282
## 10              10 1               10        74         8.27         0.0282
## # ... with 190 more rows, and 10 more variables: int_factor_2 <dbl>,
## #   slope_factor_2 <dbl>, year <int>, age <int>, cog_1 <dbl>, cog_2 <dbl>,
## #   cog_3 <dbl>, phys_1 <dbl>, phys_2 <dbl>, phys_3 <dbl>
```

```r
ds_long <-
  ds %>%
  dplyr::select(
    subject_wave_id,
    subject_id,
    wave_id,
    year,
    age,
    cog_1,
    cog_2,
    cog_3,
    phys_1,
    phys_2,
    phys_3
  ) %>%
  tidyr::gather(
    key   = manifest,
    value = value,
    -subject_wave_id, -subject_id, -wave_id, -year, -age
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
ggplot(ds, aes(x=year, y=cog_1, color=subject_id)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position="none")
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-4.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
# OuhscMunge::verify_value_headstart(ds)
checkmate::assert_integer( ds$subject_wave_id   , any.missing=F , lower=1, upper=200     , unique=T)
checkmate::assert_factor(  ds$subject_id        , any.missing=F                          )
checkmate::assert_integer( ds$wave_id           , any.missing=F , lower=1, upper=10      )
checkmate::assert_integer( ds$year              , any.missing=F , lower=2000, upper=2014 )
checkmate::assert_integer( ds$age               , any.missing=F , lower=70, upper=85     )

checkmate::assert_numeric( ds$int_factor_1      , any.missing=F , lower=4, upper=20      )
checkmate::assert_numeric( ds$slope_factor_1    , any.missing=F , lower=-1, upper=1      )
checkmate::assert_numeric( ds$int_factor_2      , any.missing=F , lower=6, upper=20      )
checkmate::assert_numeric( ds$slope_factor_2    , any.missing=F , lower=0, upper=1       )

checkmate::assert_numeric( ds$cog_1             , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$cog_2             , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$cog_3             , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$phys_1            , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$phys_2            , any.missing=F , lower=0, upper=20      )
checkmate::assert_numeric( ds$phys_3            , any.missing=F , lower=0, upper=20     )
```

```r
# dput(colnames(ds)) # Print colnames for line below.
columns_to_write <- c(
  "subject_wave_id", "subject_id",
  "wave_id", "year", "age",
  "int_factor_1", "slope_factor_1",
  "cog_1", "cog_2", "cog_3",
  "phys_1", "phys_2", "phys_3"
)
ds_slim <-
  ds %>%
  # dplyr::slice(1:100) %>%
  dplyr::select(!!columns_to_write)
ds_slim
```

```
## # A tibble: 200 x 13
##    subject_wave_id subject_id wave_id  year   age int_factor_1
##              <int> <fct>        <int> <int> <int>        <dbl>
##  1               1 1                1  2000    74         8.27
##  2               2 1                2  2001    75         8.27
##  3               3 1                3  2002    76         8.27
##  4               4 1                4  2003    77         8.27
##  5               5 1                5  2004    78         8.27
##  6               6 1                6  2005    79         8.27
##  7               7 1                7  2006    80         8.27
##  8               8 1                8  2007    81         8.27
##  9               9 1                9  2008    82         8.27
## 10              10 1               10  2009    83         8.27
## # ... with 190 more rows, and 7 more variables: slope_factor_1 <dbl>,
## #   cog_1 <dbl>, cog_2 <dbl>, cog_3 <dbl>, phys_1 <dbl>, phys_2 <dbl>,
## #   phys_3 <dbl>
```

```r
rm(columns_to_write)
```

```r
# If there's no PHI, a rectangular CSV is usually adequate, and it's portable to other machines and software.
readr::write_csv(ds_slim, config$path_mlm_1)
# readr::write_rds(ds_slim, path_out_unified, compress="gz") # Save as a compressed R-binary file if it's large or has a lot of factors.
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
## [10] bit_1.1-14            evaluate_0.12         gtable_0.2.0         
## [13] RSQLite_2.1.1         memoise_1.1.0         tibble_1.4.2         
## [16] checkmate_1.8.9-9000  lattice_0.20-38       pkgconfig_2.0.2      
## [19] rlang_0.3.0.1         cli_1.0.1             DBI_1.0.0            
## [22] rstudioapi_0.8        yaml_2.2.0            stringr_1.3.1        
## [25] knitr_1.20            withr_2.1.2           dplyr_0.7.8          
## [28] hms_0.4.2.9001        bit64_0.9-7           grid_3.5.1           
## [31] tidyselect_0.2.5      OuhscMunge_0.1.9.9009 glue_1.3.0           
## [34] R6_2.3.0              fansi_0.4.0           tidyr_0.8.2          
## [37] readr_1.2.1           purrr_0.2.5           blob_1.1.1           
## [40] scales_1.0.0          backports_1.1.2       assertthat_0.2.0     
## [43] testit_0.8.1          colorspace_1.3-2      labeling_0.3         
## [46] config_0.3            utf8_1.1.4            stringi_1.2.4        
## [49] lazyeval_0.2.1        munsell_0.5.0         markdown_0.8         
## [52] crayon_1.3.4          zoo_1.8-4
```

```r
Sys.time()
```

```
## [1] "2018-11-24 12:54:30 CST"
```

