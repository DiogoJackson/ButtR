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
# Get data only for Delias aganippe
get_Oz_butterflies(species = "Delias aganippe")
```

Download data for a specific genus:

``` r
# Get data for all species of the genus Delias
get_Oz_butterflies(genus = "Delias")
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

Download data for male *Delias aganippe* and *Delias mysis* from Brisbane
and Sydney:

``` r
# Get data with multiple filters 
get_Oz_butterflies(sex = "male", species = c("Delias aganippe", "Delias mysis"), site = c("Brisbane", "Sydney")
```

------------------------------------------------------------------------

## 📑 Summary of database content

### Folder structure
The Oz Butterflies Database has five folders for butterfly families (Papilionidae, Nymphalidae, Lycaenidae, Hesperiidae, Pieridae). Each family folder contains subfolders for each butterfly species. Each species subfolder contains subfolders for each butterfly specimen. Each specimen subfolder contains data and image files for that specimen, as shown in the schema below:

-   📁 Pieridae
    -   📁 Eurema_hecabe
        -   📁 1
            -   📄 1-v-RGB.ARW (Ventral wing surfaces photographed in
                visible light)
            -   📄 1-d-RGB.ARW (Dorsal wing surfaces photographed in
                visible light)
            -   📄 1-v-UV.ARW (Ventral wing surfaces photographed in
                ultraviolet light)
            -   📄 1-d-UV.ARW (Dorsal wing surfaces photographed in
                ultraviolet light)
            -   📄 1-RGB.ARW (Dorsal and ventral wing surfaces
                photographed in visible light - non-pinned)
            -   📄 1-UV.ARW (Dorsal and ventral wing surfaces
                photographed in ultraviolet light - non-pinned)
            -   📄 1-f.gb (CO1 gene sequenced forward - DNA)
            -   📄 1-d.jpeg (Reference image for speced patch
                numbers - dorsal)
            -   📄 1-v.jpeg (Reference image for speced patch
                numbers - ventral)
            -   📄 1-d-1-s.procspec (Spectra measured at 45 degrees -
                dorsal, patch 1)
            -   📄 1-d-1-a.procspec (Spectra measured at 60 degrees -
                dorsal, patch 1)
            -   📄 1-v-2-s.procspec (Spectra measured at 45 degrees -
                ventral, patch 2)
            -   📄 1.csv (CSV file containing all procspec measures)
            -   📄 1-s.png (Graph of the spectra measured at 45
                degrees)
            -   📄 1-a.png (Graph of the spectra measured at 60
                degrees or more)
        -   📁 2
        -   📁 3
    -   📁 Lampides_boeticus
        -   📁 4
        -   📁 5

## 💡 Citation

If you use this database in a publication, please cite it as follows:

📌 **[Citation to be added]**

------------------------------------------------------------------------

## 🤝 Helping us to improve

We welcome contributions! If you’d like to improve `buttR`, feel free
to open an **issue** for bug reports or feature requests.

------------------------------------------------------------------------
