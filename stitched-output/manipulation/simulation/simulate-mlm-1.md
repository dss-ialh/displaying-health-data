



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
loadings_factor_1   <- c(.4, .5, .6)
sigma_factor_1      <- c(.1, .2, .1)
possible_year_start <- 2000:2005
```



```r
ds_subject <-
  tibble::tibble(
    subject_id      = factor(seq_len(subject_count)),
    year_start      = sample(possible_year_start, size=subject_count, replace=T),

    int_factor_1    = rnorm(n=subject_count, mean=10.0, sd=2.0),
    slope_factor_1  = rnorm(n=subject_count, mean= 0.05, sd=0.04)
  )
ds_subject
```

```
## # A tibble: 20 x 4
##    subject_id year_start int_factor_1 slope_factor_1
##    <fct>           <int>        <dbl>          <dbl>
##  1 1                2000        10.4        0.0223  
##  2 2                2000        12.2        0.00597 
##  3 3                2004         4.58       0.0705  
##  4 4                2001         9.35       0.159   
##  5 5                2005        12.2        0.0833  
##  6 6                2003        10.3        0.0180  
##  7 7                2005        12.0        0.00520 
##  8 8                2000        10.2       -0.000935
##  9 9                2002         8.51       0.0995  
## 10 10               2004        11.3        0.0803  
## 11 11               2000         8.27       0.0282  
## 12 12               2001        11.4        0.0514  
## 13 13               2005        12.4        0.108   
## 14 14               2003         6.10       0.0616  
## 15 15               2004         7.40       0.0810  
## 16 16               2000         6.85      -0.0462  
## 17 17               2004         7.72       0.0257  
## 18 18               2001        13.1       -0.00384 
## 19 19               2001        13.4        0.0370  
## 20 20               2004        10.1        0.0143
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
```

```
## # A tibble: 200 x 8
##    subject_id wave_id int_factor_1 slope_factor_1  year cog_1 cog_2 cog_3
##    <fct>        <int>        <dbl>          <dbl> <int> <dbl> <dbl> <dbl>
##  1 1                1         10.4         0.0223  2000  4.21  4.93  6.23
##  2 1                2         10.4         0.0223  2001  4.11  5.47  6.29
##  3 1                3         10.4         0.0223  2002  4.29  5.15  6.28
##  4 1                4         10.4         0.0223  2003  4.30  4.79  6.26
##  5 1                5         10.4         0.0223  2004  4.40  5.48  6.23
##  6 1                6         10.4         0.0223  2005  4.10  5.37  6.27
##  7 1                7         10.4         0.0223  2006  4.24  5.32  6.35
##  8 1                8         10.4         0.0223  2007  4.26  5.31  6.59
##  9 1                9         10.4         0.0223  2008  4.59  5.43  6.52
## 10 1               10         10.4         0.0223  2009  4.43  5.40  6.36
## # ... with 190 more rows
```

```r
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
```

```r
library(ggplot2)

ggplot(ds_long, aes(x=year, y=value, color=subject_id)) +
  geom_line() +
  facet_wrap("manifest", ncol=3) +
  theme_minimal() +
  theme(legend.position="none")
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-1.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
ggplot(ds, aes(x=year, y=cog_1, color=subject_id)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position="none")
```

<img src="stitched-output/manipulation/simulation/simulate-mlm-1/inspect-2.png" title="plot of chunk inspect" alt="plot of chunk inspect" style="display: block; margin: auto;" />

```r
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
```

```
## # A tibble: 200 x 8
##    subject_id wave_id  year int_factor_1 slope_factor_1 cog_1 cog_2 cog_3
##    <fct>        <int> <int>        <dbl>          <dbl> <dbl> <dbl> <dbl>
##  1 1                1  2000         10.4         0.0223  4.21  4.93  6.23
##  2 1                2  2001         10.4         0.0223  4.11  5.47  6.29
##  3 1                3  2002         10.4         0.0223  4.29  5.15  6.28
##  4 1                4  2003         10.4         0.0223  4.30  4.79  6.26
##  5 1                5  2004         10.4         0.0223  4.40  5.48  6.23
##  6 1                6  2005         10.4         0.0223  4.10  5.37  6.27
##  7 1                7  2006         10.4         0.0223  4.24  5.32  6.35
##  8 1                8  2007         10.4         0.0223  4.26  5.31  6.59
##  9 1                9  2008         10.4         0.0223  4.59  5.43  6.52
## 10 1               10  2009         10.4         0.0223  4.43  5.40  6.36
## # ... with 190 more rows
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
## [1] "2018-11-24 12:13:39 CST"
```

