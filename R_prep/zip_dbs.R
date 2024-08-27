# Functions for constructing and checking the database. Carries out some checks
# of database completeness and correctness, then creates a directory in the
# format suitable for uploading to Dryad. These functions are not strictly part
# of the published package, but are used to check and publish the database.
#
# Will be slow to run the first time, because it queries the Australian Faunal
# Directory once for each species. The results are saved locally to speed up
# subsequent runs. Results are saved in AFD-query.csv in the top level package
# directory.


# Location of the source (unpacked) database to be checked
DBDIR <- "tests/testthat/testdata/db"

# Directory where the packed database will be created
REPODIR <- "tests/testthat/testdata/repo"

# Basename of the metadata spreadsheet file
METADATA_BASENAME <- "Oz_butterflies"


library(rvest) # Required for checking species names


readDbMetadata <- function(dir) {
  meta <- file.path(dir, paste0(METADATA_BASENAME, ".xlsx"))
  if (!file.exists(meta)) {
    stop(sprintf("Metadata spreadsheet (%s) not found", meta))
  }
  readxl::read_xlsx(meta, skip = 1, .name_repair = "minimal")
}

isCapitalised <- function(word) {
  grepl("^[[:upper:]][[:lower:]]+$", word)
}

isLower <- function(word) {
  grepl("^[[:lower:]]+$", word)
}

capWord <- function(word) {
  paste0(toupper(substring(word, 1, 1)),
         tolower(substring(word, 2)))
}

reportBad <- function(msg, errs, limit = 5) {
  if (length(errs) > limit) {
    cat(sprintf("%s: %s and %d more\n", msg, paste(errs[1:limit], collapse = ", "), length(errs) - limit))
  } else if (length(errs) > 0) {
    cat(sprintf("%s: %s\n", msg, paste(errs, collapse = ", ")))
  }
}

# Check the contents and structure of an unpacked database
checkOzButtUnpacked <- function(dir) {

  descr <- readDbMetadata(dir)

  # Check spelling conventions
  badFamilies <-  unique(descr$Family[!isCapitalised(descr$Family)])
  badGenera <- unique(descr$genus[!isCapitalised(descr$genus)])
  badSpecies <- unique(descr$species[!isLower(descr$species)])
  badGeneraM <- unique(descr$genus_m[!isCapitalised(descr$genus_m)])
  badSpeciesM <- unique(descr$species_m[!isLower(descr$species_m)])
  badGeneraD <- unique(descr$genus_d[!isCapitalised(descr$genus_d)])
  badSpeciesD <- unique(descr$species_d[!isLower(descr$species_d)])

  # Check that all specimens in a species have the same family
  descr$bin <- paste(descr$genus, descr$species)
  fams <- sapply(unique(descr$bin), function(sp) unique(descr$Family[descr$bin == sp]))
  badFams <- Filter(function(fams) length(fams) != 1, fams)

  # Check sex - should be "male", "female" or "unknown"
  validSex <- c("male", "female", "unknown")
  badSex <- unique(descr$sex[!descr$sex %in% validSex])

  # Check locations
  validLoc <- c("Brisbane", "Cairns", "Sydney")
  badLoc <- unique(descr$location[!descr$location %in% validLoc])

  # Check year
  validYear <- c("2022", "2023")
  badYear <- unique(descr$year[!descr$year %in% validYear])

  # Check reflectance
  validRef <- c("yes", "no")
  badRef <- unique(descr$reflectance[!descr$reflectance %in% validRef])

  # Check that every specimen in the database actually exists
  spDir <- file.path(dir, descr$Family, paste(descr$genus, descr$species, sep = "_"), descr$ID)
  badSpecimens <- which(!dir.exists(spDir))

  # Report
  reportBad("Family names not capitalised", badFamilies)
  reportBad("Genus names not capitalised", badGenera)
  reportBad("Specific names not lower case", badSpecies)
  reportBad("genus_m names not capitalised", badGeneraM)
  reportBad("species_m names not lower case", badSpeciesM)
  reportBad("genus_d names not capitalised", badGeneraD)
  reportBad("species_d names not lower case", badSpeciesD)
  reportBad("Missing specimen folder for ID", descr$ID[badSpecimens])
  reportBad("Non-unique family for species", names(badFams), limit = 4)
  reportBad(sprintf("Bad values for sex (valid values %s)", paste(validSex, collapse = ", ")), badSex, limit = 8)
  reportBad(sprintf("Bad values for location (valid values %s)", paste(validLoc, collapse = ", ")), badLoc)
  reportBad(sprintf("Bad values for year (valid values %s)", paste(validYear, collapse = ", ")), badYear)
  reportBad(sprintf("Bad values for reflectance (valid values %s)", paste(validRef, collapse = ", ")), badRef)
}

# Check the contents and structure of a packed database, i.e. in the format used for storing in Dryad
checkOzButtPacked <- function(dir) {
  meta <- file.path(dir, "Oz_butterflies.xlsx")
  if (!file.exists(meta)) {
    stop(sprintf("Metadata spreadsheet (%s) not found", meta))
  }
}

# Compares species and family names against the Australian faunal directory to check for correctness
checkSpecies <- function(dbdir) {

  queryAFDSpecies <- function(spi, species) {
    url <- sprintf("https://biodiversity.org.au/afd/taxa/%s", paste(species$genus[spi], species$species[spi], sep = "_"))

    tryCatch({
      urlc <- url(url, "rb")
      page <- read_html(urlc)
      close(urlc)

      # Check the family
      crumbs <- page |> html_element("#supertaxa-breadcrumb")
      crumbTxt <- strsplit(html_text2(crumbs), "»")[[1]]
      family <- grep("(Family)", crumbTxt, value = TRUE)
      family <- sub("^ *", "", family)
      family <- sub(" .*", "", family)
      family <- capWord(family)

      crumbs <- page |> html_element("#breadcrumb")
      adfSpecies <- (crumbs |> html_elements("em") |> html_text2())[2]

      data.frame(orig.genus = species$genus[spi], orig.species = species$species[spi],
                 orig.family = species$Family[spi],
                 afd.species = adfSpecies, afd.family = family,
                 url = url)

    },
    warning = function(e) data.frame(orig.genus = species$genus[spi], orig.species = species$species[spi],
                                     orig.family = species$Family[spi], afd.species = NA, afd.family = NA, url = url),
    error = function(e) data.frame(orig.genus = species$genus[spi], orig.species = species$species[spi],
                                   orig.family = species$Family[spi], afd.species = NA, afd.family = NA, url = url)
    )
  }

  descr <- readDbMetadata(dbdir)
  # Ignore case errors
  descr$Family <- capWord(descr$Family)
  descr$genus <- capWord(descr$genus)
  descr$species <- tolower(descr$species)
  # Limit queries to one per distinct species
  species <- unique(descr[, c("Family", "genus", "species")])

  # Try to limit queries to AFD
  if (file.exists("AFD-query.csv")) {
    afd <- read.csv("AFD-query.csv")
  } else {
    l <- lapply(seq_len(nrow(species)), queryAFDSpecies, species)
    afd <- do.call(rbind, l)
    write.csv(afd, "AFD-query.csv", row.names = FALSE)
  }

  for (spi in seq_len(nrow(species))) {
    if (is.na(species$species[spi])) {
      cat(sprintf("Unspecified species in genus %s\n", species$genus[spi]))
    } else {
      r <- afd[which(afd$orig.genus == species$genus[spi] & afd$orig.species == species$species[spi] & afd$orig.family == species$Family[spi]), ]

      if (is.na(r$afd.species)) {
        cat(sprintf("Species %s %s not found in AFD (%s)\n", species$genus[spi], species$species[spi], r$url))
      } else {
        if (r$afd.family != species$Family[spi]) {
          cat(sprintf("Wrong family for %s %s, should be %s but is %s (%s)\n",
                      species$genus[spi], species$species[spi], r$afd.family, species$Family[spi], r$url))
        }
        if (r$afd.species != paste(r$orig.genus, r$orig.species)) {
          cat(sprintf("Species %s %s doesn't match AFD species which is %s (%s)\n",
                      species$genus[spi], species$species[spi], r$afd.species, r$url))
        }
      }
    }
  }
}


#######

# Generate metadata in other formats
genMetadata <- function(dir) {
  descr <- readDbMetadata(dir)
  utils::write.csv(descr, file = file.path(dir, paste0(METADATA_BASENAME, ".csv")))
  jsonlite::write_json(descr, path = file.path(dir, paste0(METADATA_BASENAME, ".json")))
}


# Create repo structure, metadata files and species zip files
createZippedDb <- function(indir, zipDir) {
  descr <- readDbMetadata(indir)

  # Copy (and generate) meta data files
  if (!dir.exists(zipDir)) {
    dir.create(zipDir, recursive = TRUE)
  }
  file.copy(file.path(indir, paste0(METADATA_BASENAME, ".xlsx")), zipDir)
  genMetadata(zipDir)

  species <- unique(descr[, c("Family", "genus", "species")])
  speciesDir <- file.path(species$Family, paste(species$genus, species$species, sep = "_"))

  zipName <- paste(species$Family, species$genus, species$species, sep = "_")
  zipName <- paste0(zipName, ".zip")
  zipPath <- normalizePath(file.path(zipDir, zipName), mustWork = FALSE)

  origDir <- getwd()
  on.exit({ setwd(origDir) }, add = TRUE)
  # Set working directory to the parent of the Family folder so that we get
  # the relative paths in the zip file that we want
  setwd(indir)

  skippedDirs <- 0
  for (spi in seq_len(nrow(species))) {
    if (!dir.exists(speciesDir[spi])) {
      skippedDirs <- skippedDirs + 1
    } else {
      files <- list.files(speciesDir[spi], recursive = TRUE, full.names = TRUE)
      zip <- zipPath[spi]
      # Delete old zip file
      unlink(zip)
      utils::zip(zip, files)
    }
  }
  if (skippedDirs > 0) {
    cat(sprintf("Skipped %d species folders because they don't exist\n", skippedDirs))
  }
}


#######################################################

# Check the database to be packed up
checkOzButtUnpacked(DBDIR)

# Don't do this too often because it load the website
checkSpecies(DBDIR)

# Create zip files
createZippedDb(DBDIR, REPODIR)


