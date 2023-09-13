Repository of macroeconomic charts
================
Franx X. Mohr

## Introduction

This is a code repository for frequently used plots on macroeconomic
topics using only publicly available data.

The intention is to support knowledge sharing and save time programming.

## Finding code

### Copy the repository

Create a local copy of the repository and open the project.

### Search for tags

``` r
# Specify the topics you are intrested in
tags <- c("rre")

# Load list of tags and associated scripts
tagindex <- read.csv("tagindex.csv")
result <- NULL
for (i in tags) {
  temp <- dplyr::filter(tagindex, tag == i)
  result <- dplyr::bind_rows(result, temp)
}

# Show relevant files
result
```

    ##   tag                       file
    ## 1 rre     credit_new_by_sector.R
    ## 2 rre credit_stock_composition.R

## Adding a new script

Add code for a single plot in a new .R-file in the folder `scripts`.

Tag the code using just a comment in the first (!) line of the new
script.

Finally, update the index.

``` r
source("functions/update_index.R")
update_index()
```

Feel free to create a pull request, if you want to share your code.
