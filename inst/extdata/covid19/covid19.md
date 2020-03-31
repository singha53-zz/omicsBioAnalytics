
# Steps to reproduce the COVID-19 case study data

## 1\) download demographics using the GEOQuery Rpackage

``` r
g <- getGEO("GSE147507")
names(g)
```

    ## [1] "GSE147507_series_matrix.txt.gz"

``` r
e1 <- g[["GSE147507_series_matrix.txt.gz"]]

## assay characteristics
phenoData <- pData(e1)
dim(phenoData)
```

    ## [1] 20 48

``` r
rownames(phenoData) <- make.names(sapply(strsplit(as.character(phenoData$description), "_"), function(i){i[[1]]}))

## transcriptomics
mrna <- as.matrix(read.csv(here::here("inst", "extdata", "covid19", "GSE147507_RawReadCounts.tsv"), row.names = 1, sep = "\t"))
colnames(mrna) <- gsub("_", ".", colnames(mrna))

## check if both datasets are in the same order
all(rownames(phenoData) == colnames(mrna))
```

    ## [1] TRUE

``` r
demo <- setNames(phenoData[, c("cell type:ch1", "time point:ch1", "treatment:ch1")], c("cell", "time", "treatment")) %>% 
  mutate(group = paste(time, treatment, sep="_"))
```

## filter genes with zero variance and each sample must have a count of at least 5

### before filtering

``` r
nrow(mrna);
```

    ## [1] 23710

### after filtering

``` r
mrna <- mrna[apply(mrna, 1, sd) !=0, ]
mrna[mrna < 6] <- NA
mrna <- t(na.omit(mrna))
dim(mrna);
```

    ## [1]    20 10563

## Save package data

``` r
## save covid19 datasets
covid19 <- list(
  demo = demo,
  mrna = mrna
)

usethis::use_data(covid19, overwrite = TRUE)
```

    ## ✔ Setting active project to '/Users/asingh/Documents/omicsBioAnalytics'
    ## ✔ Saving 'covid19' to 'data/covid19.rda'

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
    ## [1] parallel  stats     graphics  grDevices utils     datasets  methods  
    ## [8] base     
    ## 
    ## other attached packages:
    ##  [1] GEOquery_2.52.0     Biobase_2.44.0      BiocGenerics_0.30.0
    ##  [4] knitr_1.24          forcats_0.4.0       stringr_1.4.0      
    ##  [7] dplyr_0.8.3         purrr_0.3.3         readr_1.3.1        
    ## [10] tidyr_0.8.3         tibble_2.1.3        ggplot2_3.2.1      
    ## [13] tidyverse_1.2.1    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] clisymbols_1.2.0 tidyselect_0.2.5 xfun_0.9         haven_2.1.1     
    ##  [5] lattice_0.20-38  colorspace_1.4-1 generics_0.0.2   vctrs_0.2.1     
    ##  [9] usethis_1.5.1    htmltools_0.4.0  yaml_2.2.0       rlang_0.4.0     
    ## [13] pillar_1.4.2     glue_1.3.1       withr_2.1.2      modelr_0.1.5    
    ## [17] readxl_1.3.1     munsell_0.5.0    gtable_0.3.0     cellranger_1.1.0
    ## [21] rvest_0.3.4      evaluate_0.14    curl_4.0         broom_0.5.2     
    ## [25] Rcpp_1.0.2       scales_1.0.0     backports_1.1.4  limma_3.40.6    
    ## [29] jsonlite_1.6     fs_1.3.1         hms_0.5.1        digest_0.6.23   
    ## [33] stringi_1.4.3    rprojroot_1.3-2  grid_3.6.1       here_0.1        
    ## [37] cli_1.1.0        tools_3.6.1      magrittr_1.5     lazyeval_0.2.2  
    ## [41] crayon_1.3.4     pkgconfig_2.0.2  zeallot_0.1.0    xml2_1.2.2      
    ## [45] lubridate_1.7.4  assertthat_0.2.1 rmarkdown_1.15   httr_1.4.1      
    ## [49] rstudioapi_0.10  R6_2.4.0         nlme_3.1-140     compiler_3.6.1
