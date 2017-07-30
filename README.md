# EbayesThresh: Empirical Bayes thresholding in R

[![CRAN status badge](http://www.r-pkg.org/badges/version/EbayesThresh)](https://cran.r-project.org/package=EbayesThresh)
[![Travis CI Build Status](https://travis-ci.org/stephenslab/EbayesThresh.svg?branch=master)](https://travis-ci.org/stephenslab/EbayesThresh)
[![Appveyor Build status](https://ci.appveyor.com/api/projects/status/0lmh3taxi4etdtix?svg=true)](https://ci.appveyor.com/project/pcarbo/ebayesthresh)

The EbayesThresh package provides a simple interface for Empirical
Bayes thresholding using the methods developed by I. M. Johnstone and
B. W. Silverman.

## License

The *EbayesThresh* source code repository is free software: you can
redistribute it under the terms of the
[GNU General Public License](http://www.gnu.org/licenses/gpl.html). All
the files in this project are part of *EbayesThresh*. This project is
distributed in the hope that it will be useful, but **without any
warranty**; without even the implied warranty of **merchantability or
fitness for a particular purpose**. See file [LICENSE](LICENSE) for
the full text of the license.

## Quick Start

Install the [latest
release](https://github.com/stephenslab/EbayesThresh/releases/tag/v1.4-11)
of the EbayesThresh package using `devtools`:

```R
library(devtools)
install_github("stephenslab/EbayesThresh@v1.4-11")
```

Load EbayesThresh into your R environment, and get an overview of the
package:

```R
library(EbayesThresh)
help(package = EbayesThresh)
```

Explore the EbayesThresh vignette, either
[on the Web][vignette],
or in R:

```R
vignette("ebayesthresh")
```

## What's included

This is the current structure of the R package:

```bash
├── DESCRIPTION
├── LICENSE
├── NAMESPACE
├── R
├── README.md
├── inst
│   └── kan-xu-thesis.pdf
├── man
├── tests
│   ├── testthat
│   └── testthat.R
└── vignettes
    └── ebayesthresh.Rmd
```

## Credits 

The *EbayesThresh* software package was originally developed by
Bernard W. Silverman and Ludger Evers, with extensions introduced by
Kan Xu, Peter Carbonetto and Matthew Stephens in the Department of
Statistics at the University of Chicago.

[vignette]: https://stephenslab.github.io/EbayesThresh/vignettes/ebayesthresh.html
