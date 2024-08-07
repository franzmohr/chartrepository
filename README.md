Code repository for macroeconomic charts
================

## Introduction

### What this repository tries to do

This is a code repository for scripts, which generate frequently used
plots on macroeconomic topics based only on publicly available data. The
intention is to support knowledge sharing and save programming time.

The repository is platform agnostic, though `R` and `Python` code would
be preferred.

An overview of available charts can be found in subfolder
[`figures`](/figures).

### Motivation

At some point at work you start to lose track of the charts you have
made over the years. You might need to update some charts very regularly
and know where to find your highly optimized code. However, there are
other charts, which you know you made some years ago, but you cannot
find the code anymore. So you start creating a code repository. But how
should you organize it?

- By topic? But some charts are used for multiple topics. For example,
  macroeconomic overviews as well as real estate factsheets might use
  visualisations of house prices. So where to you put the code for those
  charts, unless you want to duplicate it?
- By data source? But how do you find all charts used for a specific
  topic, if you are not aware of all the relevant data sources?

In this repository, I propose tags embedded in the first line of a
script. Those tags can be searched and no further structuring of the
scripts is necessary. This ensures that the code for one chart is only
included once and thematic overlaps are possible.

## Usage

### Finding code

- Create a local copy of this repository and open the project.
- You can use the following code to search the list of tags for a
  specific topic

``` r
# Specify the topics you are interested in
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

    ##   tag                                          file
    ## 1 rre     bank_credit_growth_by_sector_line_chart.R
    ## 2 rre                   bank_credit_new_by_sector.R
    ## 3 rre               bank_credit_stock_composition.R
    ## 4 rre bank_household_rre_credit_growth_line_chart.R
    ## 5 rre                         bank_interest_rates.R
    ## 6 rre           rre_price_growth_for_eu_countries.R
    ## 7 rre            rre_rent_growth_for_eu_countries.R

- Use the code in the R-files to make your own chart

### Adding a new script

- Add code for a single plot in a new R script in the folder `scripts`.
- Save the resulting chart in a png file in folder `figures`. The file
  should have the same name as the R script.
- Tag the code using just a comment in the first (!) line of the new
  script.
- Update the index using the following lines

``` r
source("functions/update_index.R")
update_index()
```

- Feel free to create a pull request, if you want to share your code.

## Contributors

- [Franz X. Mohr](https://github.com/franzmohr)
- [Elizabeth
  Eckel](https://www.linkedin.com/in/elizabeth-eckel-5561b419a)
