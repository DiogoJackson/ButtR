<!-- badges: start -->
[![R-CMD-check](https://github.com/DiogoJackson/ButtR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiogoJackson/ButtR/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/DiogoJackson/ButtR/graph/badge.svg)](https://app.codecov.io/gh/DiogoJackson/ButtR)
<!-- badges: end -->

# ButtR

R package to download and install all or part of the Oz butterflies database.

The Oz butterflies database can be downloaded manually, however it is simpler to
use this package to do so. The database is quite large, so if the
entire database is not required, `buttR` provides an efficient mechanism to download
and install only the desired parts of the database.

If you use the Oz butterflies database, please cite the paper:

TODO

## Installation

    $ install.packages("buttR")

Or to install the latest development version directly from Github:

    $ install.packages("devtools") # only if not already installed
    $ devtools::install_github("DiogoJackson/ButtR")

## Summary of database contents

* Folder structure
* Metadata
* Data files

## Examples of use

TODO

    $ # Install the entire Oz butterflies database in a folder called Oz_butterflies
    $ get_species()
    
    $ Download all data on specimens with species "Zizina otis"
    $ get_species(species = "Zizina otis")

## R_prep folder

This repository also contains R code used during the construction of 
the database, which is not part of the R package. It is included here 
so that all R code is maintained in a single repository.
