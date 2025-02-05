# 🦋 ButtR 🦋

[![R-CMD-check](https://github.com/DiogoJackson/ButtR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/DiogoJackson/ButtR/actions/workflows/R-CMD-check.yaml)\
[![Codecov test
coverage](https://codecov.io/gh/DiogoJackson/ButtR/graph/badge.svg)](https://app.codecov.io/gh/DiogoJackson/ButtR)

## Overview

`ButtR` is an R package designed to simplify the download and handling
of the **Oz Butterflies Database**, a large dataset containing
comprehensive records of reflectance spectra and photography of
Australian butterflies.

The Oz butterflies database can be downloaded manually from [Butterfly
database site](https://blog.datadryad.org). However, it is simpler to
use this package to do so. The database is quite large, so if the entire
database is not required, `buttR` provides an efficient mechanism to
download and install only the desired parts of the database.

If you use the Oz butterflies database, please cite the paper: \
📌 **[Citation to be added]**

## Why Use `ButtR`?

✅ **Easy Download** – Quickly access butterfly data with a single
command .\
✅ **Selective Filtering** – Download only specific species, families, or
locations, saving disk space and processing time.

## 🛠 Functions

The `ButtR` package provides the following core functions:

| Function | Description |
|---------------------------------|---------------------------------------|
| `get_Oz_butterflies()` | Downloads the dataset, optionally filtered by species, family, or location |

## 📥 Installation

You can install the stable version from CRAN:

``` r
#install the package 
install.packages("ButtR")

#load the package 
library("ButtR") 
```

Or install the latest development version from GitHub. In this case, you
need to have the `devtools` package installed:

``` r
# Install devtools if not already installed 
install.packages("devtools") 
library("devtools")

# Install ButtR from GitHub 
devtools::install_github("DiogoJackson/ButtR") 
library("ButtR")
```

## 🚀 Examples of use

After installation and activation, you can use the
*get_Oz_butterflies()* function from `ButtR` to download the entire
database or filter specific subsets.

Download the entire database:

``` r
# Download the full Oz Butterflies Database
get_Oz_butterflies()
```

Download data for a specific species:

``` r
# Get data only for Zizina otis
get_Oz_butterflies(species = "Zizina otis")
```

Download data for a specific genus:

``` r
# Get data only for Zizina otis
get_Oz_butterflies(genus = "Zizina")
```

Download data for a specific family:

``` r
# Get all species within the Nymphalidae family
get_Oz_butterflies(family = "Nymphalidae")
```

Download data by site:

``` r
# Get all butterfly species from Sydney
get_Oz_butterflies(site = "Sydney")
```

Download data for male *Zizina otis* and *Zizina labradus* from Brisbane
and Sydney:

``` r
# Get data with multiple filters 
get_Oz_butterflies(sex = "male", species = c("Zizina otis", "Zizina labradus"), site = c("Brisbane", "Sydney")
```

------------------------------------------------------------------------

## 📑 Summary of database content

### Folder structure
TODO
### Metadata
TODO
### Data files
TODO

## 💡 Citation

If you use this database in a publication, please cite it as follows:

📌 **[Citation to be added]**

------------------------------------------------------------------------

## 🤝 Helping us to improve

We welcome contributions! If you’d like to improve `buttR`, feel free
to open an **issue** for bug reports or feature requests.

------------------------------------------------------------------------

## 📜 License

This package is released under the **MIT License**.

Copyright (c) 2024 Diogo J. A. Silva, Jim McLean

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

------------------------------------------------------------------------
