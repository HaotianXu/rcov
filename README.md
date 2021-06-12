
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- [![Travis-CI Build Status](https://travis-ci.org/SMAC-Group/simts.svg?branch=master)](https://travis-ci.org/SMAC-Group/simts) -->

<!-- [![Licence](https://img.shields.io/badge/licence-AGPL--3.0-blue.svg)](https://opensource.org/licenses/AGPL-3.0) -->

<!-- [![minimal R version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/) -->

<!-- [![CRAN](http://www.r-pkg.org/badges/version/simts)](https://cran.r-project.org/package=simts) -->

<!-- [![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/simts)](http://www.r-pkg.org/pkg/simts) -->

<!-- [![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/grand-total/simts)](http://www.r-pkg.org/pkg/simts) -->

<!-- [![Last-changedate](https://img.shields.io/badge/last%20change-2021--06--12-green.svg)](https://github.com/SMAC-Group/simts) -->

# `rcov` Overview

The robust autocovariance matrix estimation (`rcov`) R package provides
a series of tools to obtain the robust autocovariance matrix estimators.
Its original purpose was to be a support to the paper \[“Nonasymptotic
theories for tail-robust autocovariance matrix estimation methods”\] but
can obviously be used for time series analysis in general. More
specifically, the package provides tools with the following features:

  - Simulation of multivariate stationary time series from linear
    process model and VAR(1) model.
  - Robust estimation of mean vectors for high-dimensional stationary
    time series.
  - Robust estimation of autocovariance matrices for high-dimensional
    stationary time series.
  - Robustification parameter selection based on the block-wise
    cross-validation.

## Install Instructions

## Installation

The `rcov` package is available on GitHub. You can install the stable
version of the `rcov` package with:

``` r
# Install dependencies
install.packages(c("RcppArmadillo","devtools","knitr","rmarkdown"))

# Install the package from GitHub
devtools::install_github("HaotianXu/rcov")
```

*The setup to obtain the development version of `rcov` is platform
dependent.*

## License

The license this source code is released under is the GNU AFFERO GENERAL
PUBLIC LICENSE (AGPL) v3.0. Please see the LICENSE file for full text.
Otherwise, please consult [TLDR
Legal](https://tldrlegal.com/license/gnu-affero-general-public-license-v3-\(agpl-3.0\))
or [GNU](https://www.gnu.org/licenses/agpl-3.0.en.html) which will
provide a synopsis of the restrictions placed upon the code.

<!-- ### Requirements and Dependencies -->

<!-- **OS X** -->

<!-- Some users report the need to use X11 to suppress shared library errors. To install X11, visit [xquartz.org](http://www.xquartz.org/). -->

<!-- **Linux** -->

<!-- Both curl and libxml are required. -->

<!-- For **Debian** systems, enter the following in terminal: -->

<!-- ```{r, eval = F, engine='bash'} -->

<!-- sudo apt-get install curl libcurl3 libcurl3-dev libxml2 libxml2-dev -->

<!-- ``` -->

<!-- For **RHEL** systems, enter the following in terminal: -->

<!-- ```{r, eval = F, engine='bash'} -->

<!-- sudo yum install curl curl-devel libxml2 libxml2-dev -->

<!-- ``` -->
