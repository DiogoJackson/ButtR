# Functions for constructing and checking the database. Carries out some basic
# checks of database completeness and correctness. These functions are not
# strictly part of the published package, but are used to check and publish the
# database.

METADATA_BASENAME <- "Oz_butterflies"

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

DBDIR <- "tests/testthat/testdata/db"
REPODIR <- "tests/testthat/testdata/repo"

# Check the database to be packed up
checkOzButtUnpacked(DBDIR)

# Create zip files
createZippedDb(DBDIR, REPODIR)


