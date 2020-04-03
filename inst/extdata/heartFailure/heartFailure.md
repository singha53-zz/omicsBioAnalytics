
## Steps to reproduce the heart failure case study data

  - download demographics and omics data from
    <https://amritsingh.shinyapps.io/multiomics_HFhospitalizations/>
  - reduce the transcripts dataset to 5000 features (10MB to \<5MB)
  - delete the large trascripts file to reduce package size

<!-- end list -->

``` r
# transcripts <- read.csv(here::here("inst", "extdata", "heartFailure", "transcripts.tsv"), header = TRUE, sep = "\t")
# geneSd <- apply(transcripts, 2, sd)
# write.table(transcripts[, order(geneSd, decreasing = TRUE)[1:5000]], file = here::here("inst", "extdata", "caseStudy", "mrna.tsv"), sep = "\t")

omicsData <- lapply(c("cells.tsv", "holter.tsv", "mrna.tsv", "proteins.tsv"), function(i){
  read.csv(here::here("inst", "extdata", "heartFailure", i), header = TRUE, sep = "\t")
})
names(omicsData) <- c("cells", "holter", "mrna", "proteins")

## save heart failure datasets
heartFailure <- list(
  demo = read.csv(here::here("inst", "extdata", "heartFailure", "clinical.tsv"), header = TRUE, sep = "\t"),
  omicsData = omicsData
)
```

## Save package data

``` r
usethis::use_data(heartFailure, overwrite = TRUE)
```

    ## ✔ Setting active project to '/Users/asingh/Documents/omicsBioAnalytics'
    ## ✔ Saving 'heartFailure' to 'data/heartFailure.rda'

## Session Info

``` r
sessionInfo()
```

    ## R version 3.6.1 (2019-07-05)
    ## Platform: x86_64-apple-darwin15.6.0 (64-bit)
    ## Running under: macOS Catalina 10.15.3
    ## 
    ## Matrix products: default
    ## BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
    ## LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib
    ## 
    ## locale:
    ## [1] en_CA.UTF-8/en_CA.UTF-8/en_CA.UTF-8/C/en_CA.UTF-8/en_CA.UTF-8
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ##  [1] knitr_1.24      forcats_0.4.0   stringr_1.4.0   dplyr_0.8.3    
    ##  [5] purrr_0.3.3     readr_1.3.1     tidyr_0.8.3     tibble_2.1.3   
    ##  [9] ggplot2_3.2.1   tidyverse_1.2.1
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] clisymbols_1.2.0 tidyselect_0.2.5 xfun_0.9         haven_2.1.1     
    ##  [5] lattice_0.20-38  colorspace_1.4-1 generics_0.0.2   vctrs_0.2.1     
    ##  [9] htmltools_0.4.0  usethis_1.5.1    yaml_2.2.0       rlang_0.4.0     
    ## [13] pillar_1.4.2     glue_1.3.1       withr_2.1.2      modelr_0.1.5    
    ## [17] readxl_1.3.1     munsell_0.5.0    gtable_0.3.0     cellranger_1.1.0
    ## [21] rvest_0.3.4      evaluate_0.14    broom_0.5.2      Rcpp_1.0.2      
    ## [25] scales_1.0.0     backports_1.1.4  jsonlite_1.6     fs_1.3.1        
    ## [29] hms_0.5.1        digest_0.6.23    stringi_1.4.3    grid_3.6.1      
    ## [33] rprojroot_1.3-2  here_0.1         cli_1.1.0        tools_3.6.1     
    ## [37] magrittr_1.5     lazyeval_0.2.2   crayon_1.3.4     pkgconfig_2.0.2 
    ## [41] zeallot_0.1.0    xml2_1.2.2       lubridate_1.7.4  assertthat_0.2.1
    ## [45] rmarkdown_1.15   httr_1.4.1       rstudioapi_0.10  R6_2.4.0        
    ## [49] nlme_3.1-140     compiler_3.6.1
