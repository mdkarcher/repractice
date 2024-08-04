
<!-- README.md is generated from README.Rmd. Please edit that file -->

# repractice

<!-- badges: start -->
<!-- badges: end -->

The goal of `repractice` is to facilitate the authoring and of
combinatorially variable practice problems and solutions.

## Installation

You can install the development version of repractice like so:

``` r
devtools::install_github("mdkarcher/repractice")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(repractice)

details <- list(name = "Michael", subject="statistics")
compile_text("My name is <<name>>, and I teach <<subject>>.", vars = details)
#> My name is Michael, and I teach statistics.

setup <- list(exponent = 3)
compile_text(r"(The derivative of $\pm C x^<<exponent>>$ is $\pm <<exponent>> C x^<<exponent-1>>$)", vars = setup)
#> The derivative of $\pm C x^3$ is $\pm 3 C x^2$
```
