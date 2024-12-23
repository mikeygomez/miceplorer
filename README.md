
<!-- README.md is generated from README.Rmd. Please edit that file -->

``` r
knitr::include_graphics("man/figures/miceplorer.jpg")
```

![](man/figures/miceplorer.jpg)<!-- -->

# miceplorer

<!-- badges: start -->
<!-- badges: end -->

\*Note that the hex sticker was made using ChatGPT

The goal of miceplorer is to help in cleaning and preprocessing the
data. You should use this package, because it tests for a wide variety
of potential errors including making sure that weight and outcome
measurement data are all numeric, ensuring that column names are
correct, ensuring that IDs for all mice are correct, and identifying
potential outliers. This package seeks to be a robust and easy-to-use
tool in addressing errors in your datasets.

Additionally, you should use miceplorer, because it gives you
flexibility in the way you want to clean your dataset. For a
comprehensive clean, we have a function called clean_mouse_data that
will clean all your tabs and calls individual functions for cleaning
birth data, weight data, and outcome data. By breaking it up into
different functions, you have autonomy over which tabs you want to
clean. Additionally, the cleaning functions for weight and outcome data
are also broken up into the same functions - one to check for proper
column names, one to check for correct IDs, and one to check for
quantitative variables. These functions are also available to you to
use.

This package also fits a time-series model on the data to identify
potential outliers or issues with data recording. You can do this by
calling the identifyoutliers() function. This package will also check
for mice that have had over a 20% reduction in weight from baseline and
mark them for SACing. Both these functions take the data and make into a
format where each column is a different ID and and each row is each
date. The individual function, preprocessdata(), is also available to
you. Lastly, for basic EDA, this package also lets you plot the data by
treatment group using the plot_indmeasurement() function.

## Installation

You can install the development version of miceplorer by entering this
into the console:

``` r
devtools::install_github("mikeygomez/miceplorer")
```

From there, to load the package in your current session, call:

``` r
library(miceplorer)
```

## Example

This is a basic example of how to clean your dataset.

The excel file is expected to contain three sheets:

1.  The birth data sheet sheet should contain the following columns in
    that particular order: ID, Sex, Num, Treatment

2.  The body weight data sheet sheet should contain the following
    columns in that particular order: ID, Body Weight 1, Date Body
    Weight 1, …, Body Weight X, Date Body Weight X

3.  The outcome data sheet sheet should contain the following columns in
    that particular order: ID, Outcome 1, Date Outcome 1, …, Outcome X,
    Date Outcome X

``` r
library(miceplorer)

path <- "~/miceplorer/data-raw/mousedata.xlsx"
mousedata <- clean_mouse_data(path)
```

Here is how you can visualize interim results:

``` r
treatment_info <- create_treatment_info(mousedata[1])

plots <- plot_indmeasurement(mousedata[2], "Body Weight", treatment_info)
print(plots$treatment)
print(plots$placebo)
```
