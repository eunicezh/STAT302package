
# STAT302package

<!-- badges: start -->
[![R-CMD-check](https://github.com/eunicezh/STAT302package/workflows/R-CMD-check/badge.svg)](https://github.com/eunicezh/STAT302package/actions)
[![codecov](https://codecov.io/gh/eunicezh/STAT302package/branch/master/graph/badge.svg?token=MT4R2FZ0KE)](https://codecov.io/gh/eunicezh/STAT302package)
<!-- badges: end -->

The goal of STAT302package is to use the Hypothesis Testing and Statistical Prediction Algorithms (K-nearest neighbor and Random Forest) to produce inference and prediction.


The vignette demonstrates example usage of all main functions. Please file an issue if you have a request for a tutorial that is not currently included. You can see the vignette by using the following code:


```{r}
library(STAT302package)
# Use this to view the vignette in the corncob HTML help
help(package = "STAT302package", help_type = "html")
# Use this to view the vignette as an isolated HTML file
utils::browseVignettes(package = "STAT302package")
```

## Installation

You can install the package from the following line:

``` r
devtools::install_github("eunicezh/STAT302package", build_vignette = TRUE, build_opts = c())
```


``` r
library(STAT302package)
## basic example code
```

