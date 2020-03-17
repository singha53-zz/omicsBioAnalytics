
## Steps to reproduce the heart failure case study data

  - download demographics and omics data from
    <https://amritsingh.shinyapps.io/multiomics_HFhospitalizations/>
  - reduce the transcripts dataset to 5000 features (10MB to \<5MB)

<!-- end list -->

``` r
transcripts <- read.csv(here::here("inst", "extdata", "caseStudy", "transcripts.tsv"), header = TRUE, sep = "\t")
geneSd <- apply(transcripts, 2, sd)
write.table(transcripts[, order(geneSd, decreasing = TRUE)[1:5000]], file = here::here("inst", "extdata", "caseStudy", "mrna.tsv"), sep = "\t")
```
