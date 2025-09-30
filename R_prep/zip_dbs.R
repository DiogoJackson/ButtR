# Functions for constructing and checking the database. Carries out some checks
# of database completeness and correctness, then creates a directory in the
# format suitable for uploading to a repository. These functions are not
# strictly part of the published package, but are used to check and publish the
# database.
#
# Will be slow to run the first time, because it queries the Australian Faunal
# Directory once for each species. The results are saved locally to speed up
# subsequent runs. Results are saved in AFD-query.csv in the top level package
# directory.

# install.packages("devtools")
# devtools::install_github("JimMcL/JUtils")
library(JUtils)
library(openxlsx)
library(lubridate)
library(rvest) # Required for checking species names
source("R/summarise.R")

# Determine how files get grouped together in zip files. Options for are Family, Genus or Species
ZIP_INTO <- "Genus"

# If TRUE, only generate metadata, i.e. the two spreadsheets, each in 3 formats
METADATA_ONLY <- FALSE

# Location of the source (unpacked) database to be checked
DBDIR <- "D:\\Oz_Butterflies"
# Directory where the packed database will be created
REPODIR <- "D:\\Oz_zips"


FOR_TESTING_ONLY <- FALSE
if (FOR_TESTING_ONLY) {
  message("Generating a truncated repository for testing only")

  DBDIR <- "tests/testthat/testdata/db"
  REPODIR <- "tests/testthat/testdata/repo"
}


# Basename of the metadata spreadsheet file
METADATA_BASENAME <- "Oz_butterflies"
# Basename of the summary spreadsheet file
SUMMARY_BASENAME <- "Oz_butterflies_summary"

# List of file extensions in database
ALLOWED_EXTENSIONS <- c("ARW", "jpg", "txt", "ProcSpec", "png", "ab1", "csv") # DNA extension was gb, now fastq


readDbMetadata <- function(dir) {
  meta <- file.path(dir, paste0(METADATA_BASENAME, ".csv"))
  if (!file.exists(meta)) {
    stop(sprintf("Metadata spreadsheet (%s) not found", meta))
  }
  utils::read.csv(meta)
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

trim <- function(w) { sub("^ +", "", sub(" +$", "", w)) }

reportBad <- function(msg, errs, limit = 5) {
  if (length(errs) > limit) {
    cat(sprintf("%s: %s and %d more\n", msg, paste(errs[1:limit], collapse = ", "), length(errs) - limit))
  } else if (length(errs) > 0) {
    cat(sprintf("%s: %s\n", msg, paste(errs, collapse = ", ")))
  }
}

# Returns the path of the species data for each sample in the metadata spreadsheet descr.
# @returns Vector of directory names relative to the database folder
speciesDirectory <- function(descr) {
  file.path(descr$Family, paste(descr$Genus, descr$Species, sep = "_"))
}

# Returns the path of the sample data for each sample in the metadata spreadsheet descr.
# @returns Vector of directory names relative to the database folder
sampleDirectory <- function(descr) {
  file.path(descr$Family, paste(descr$Genus, descr$Species, sep = "_"), descr$ID)
}

# Check that the image ID in the file names matches the containing folder for each image
fileMatchesSpecimen <- function(imgFiles) {
  fileId <- sub("_.*", "", basename(imgFiles))
  dirId <- basename(dirname(imgFiles))
  fileId == dirId
}

# Check the contents and structure of an unpacked database
checkOzButtUnpacked <- function(dir) {

  descr <- readDbMetadata(dir)

  # Check spelling conventions
  badFamilies <-  unique(descr$Family[!isCapitalised(descr$Family)])
  badGenera <- unique(descr$Genus[!isCapitalised(descr$Genus)])
  badSpecies <- unique(descr$Species[!isLower(descr$Species)])
  badGeneraM <- unique(descr$Genus_m[!isCapitalised(descr$genus_m)])
  badSpeciesM <- unique(descr$Species_m[!isLower(descr$species_m)])
  badGeneraD <- unique(descr$Genus_d[!isCapitalised(descr$genus_d)])
  badSpeciesD <- unique(descr$Species_d[!isLower(descr$species_d)])

  # Check that all specimens in a species have the same family
  descr$bin <- paste(descr$Genus, descr$Species)
  fams <- sapply(unique(descr$bin), function(sp) unique(descr$Family[descr$bin == sp]))
  badFams <- Filter(function(fams) length(fams) != 1, fams)

  # Check sex - should be "Male", "Female" or "Unknown"
  validSex <- c("Male", "Female", "Unknown")
  badSex <- unique(descr$Sex[!descr$Sex %in% validSex])

  # # Check locations
  # validLoc <- c("Brisbane", "Cairns", "Sydney")
  # badLoc <- unique(descr$location[!descr$location %in% validLoc])
  #
  # # Check year
  # validYear <- c("2022", "2023")
  # badYear <- unique(descr$year[!descr$year %in% validYear])

  # Bad Day
  badDay <- unique(descr$Day[is.na(dmy(descr$Day, quiet = TRUE))])

  # # Check reflectance
  # validRef <- c("yes", "no")
  # badRef <- unique(descr$reflectance[!descr$reflectance %in% validRef])

  # Check that every specimen in the database actually exists
  spDir <- file.path(dir, speciesDirectory(descr))
  badSpecimens <- which(!dir.exists(spDir))

  # Check that every folder corresponds to something in the database
  famDirs <- list.dirs(dir, recursive = FALSE, full.names = FALSE)
  badFamDirs <- famDirs[!famDirs %in% unique(descr$Family)]
  specDirs <- list.dirs(file.path(dir, famDirs), recursive = FALSE, full.names = FALSE)
  badSpecDirs <- specDirs[!sub("_", " ", specDirs) %in% unique(descr$bin)]
  # Expand bad directories so we can report family folder
  fullSpDirs <- apply(expand.grid(file.path(dir, famDirs), badSpecDirs), 1, paste, collapse = "/")
  badSpecDirs <- fullSpDirs[file.exists(fullSpDirs)]
  # Remove DBS directory from path
  badSpecDirs <- substring(badSpecDirs, nchar(dir) + 2)

  # Check file names
  files <- list.files(file.path(dir, famDirs), recursive = TRUE)
  exts <- tools::file_ext(files)
  badExts <- files[!exts %in% ALLOWED_EXTENSIONS]

  # Image files should start with the specimen ID, which should be the same as the containing folder
  imgs <- list.files(dir, pattern = "\\.ARW", recursive = TRUE, full.names = TRUE)
  badImgFiles <- imgs[!fileMatchesSpecimen(imgs)]

  # ProcSpec files should start with their specimen IDs
  specs <- list.files(dir, pattern = "\\.ProcSpec", recursive = TRUE, full.names = TRUE)
  # Ignore standard spec files (red, green blue)
  specs <- grep("standard-.*.ProcSpec", specs, invert = TRUE, value = TRUE)
  badSpecFiles <- specs[!fileMatchesSpecimen(specs)]

  # Report
  reportBad("Family names not capitalised", badFamilies)
  reportBad("Genus names not capitalised", badGenera, limit = 10)
  reportBad("Specific names not lower case", badSpecies)
  # reportBad("genus_m names not capitalised", badGeneraM)
  # reportBad("species_m names not lower case", badSpeciesM)
  # reportBad("genus_d names not capitalised", badGeneraD)
  # reportBad("species_d names not lower case", badSpeciesD)
  # reportBad("Missing specimen folder for ID", descr$ID[badSpecimens])
  reportBad("Non-unique family for species", names(badFams), limit = 4)
  reportBad(sprintf("Bad values for sex (valid values %s)", paste(validSex, collapse = ", ")), badSex, limit = 8)
  # reportBad(sprintf("Bad values for location (valid values %s)", paste(validLoc, collapse = ", ")), badLoc)
  # reportBad(sprintf("Bad values for year (valid values %s)", paste(validYear, collapse = ", ")), badYear)
  # reportBad(sprintf("Bad values for reflectance (valid values %s)", paste(validRef, collapse = ", ")), badRef)
  reportBad("Bad values for day (expected \"dd/mm/yyyy\")", badDay, limit = 8)
  reportBad("Specimen ID in image files don't match containing folder", badImgFiles, limit = 4)
  reportBad("Specimen ID in spec files don't match containing folder", badSpecFiles, limit = 4)

  reportBad("Invalid folders at family level in DBS", badFamDirs)
  reportBad("Invalid species folders in DBS", badSpecDirs)
  reportBad("File with invalid extensions", badExts)

  # Check and report y|n columns in more detail because it's hard to find the mistakes in the data
  for (cl in c("Body", "Forewing.left", "Forewing.right", "Hindwing.left",
               "Hindwing.right")) {
    v <- descr[[cl]]
    badi <- which(!v %in% c("y", "n"))
    for (li in badi) {
      cat(sprintf("Bad value '%s', column %s, butterfly ID %d\n", v[li], cl, descr$ID[li]))
    }
  }


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
    url <- sprintf("https://biodiversity.org.au/afd/taxa/%s", paste(species$Genus[spi], species$Species[spi], sep = "_"))

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

      data.frame(orig.genus = species$Genus[spi], orig.species = species$Species[spi],
                 orig.family = species$Family[spi],
                 afd.species = adfSpecies, afd.family = family,
                 url = url)

    },
    warning = function(e) data.frame(orig.genus = species$Genus[spi], orig.species = species$Species[spi],
                                     orig.family = species$Family[spi], afd.species = NA, afd.family = NA, url = url),
    error = function(e) data.frame(orig.genus = species$Genus[spi], orig.species = species$Species[spi],
                                   orig.family = species$Family[spi], afd.species = NA, afd.family = NA, url = url)
    )
  }

  descr <- readDbMetadata(dbdir)
  # Ignore case errors
  descr$Family <- capWord(descr$Family)
  descr$Genus <- capWord(descr$Genus)
  descr$Species <- tolower(descr$Species)
  # Limit queries to one per distinct species
  species <- unique(descr[, c("Family", "Genus", "Species")])

  # Try to limit queries to AFD
  if (file.exists("AFD-query.csv")) {
    afd <- utils::read.csv("AFD-query.csv")
  } else {
    l <- lapply(seq_len(nrow(species)), queryAFDSpecies, species)
    afd <- do.call(rbind, l)
    write.csv(afd, "AFD-query.csv", row.names = FALSE)
  }

  for (spi in seq_len(nrow(species))) {
    if (is.na(species$Species[spi])) {
      cat(sprintf("Unspecified species in genus %s\n", species$Genus[spi]))
    } else {
      r <- afd[which(afd$orig.genus == species$Genus[spi] & afd$orig.species == species$Species[spi] & afd$orig.family == species$Family[spi]), ]

      if (is.na(r$afd.species)) {
        cat(sprintf("Species %s %s not found in AFD (%s)\n", species$Genus[spi], species$Species[spi], r$url))
      } else {
        if (r$afd.family != species$Family[spi]) {
          cat(sprintf("Wrong family for %s %s, should be %s but is %s (%s)\n",
                      species$Genus[spi], species$Species[spi], r$afd.family, species$Family[spi], r$url))
        }
        if (r$afd.species != paste(r$orig.genus, r$orig.species)) {
          cat(sprintf("Species %s %s doesn't match AFD species which is %s (%s)\n",
                      species$Genus[spi], species$Species[spi], r$afd.species, r$url))
        }
      }
    }
  }
}

#######

hasPinnedImages <- function(sampleDirs) {
  sapply(sampleDirs, function(d) {
    pinnedFiles <- list.files(d, pattern = "_[d|v]_.*\\.ARW$")
    # There should be 0 or an even number of files
    nf <- length(pinnedFiles)
    if (nf != 0 && nf %% 2 != 0) {
      stop(sprintf("Uneven number (%d) of pinned image files in sample folder %s:\n  %s\n",
                   nf, d, paste(pinnedFiles, collapse = ", ")))
    }
    nf > 0
  })
}

hasDNA <- function(sampleDirs) {
  sapply(sampleDirs, function(d) {
    nf <- length(list.files(d, pattern = "\\.ab1"))
    nf > 0
  })
}

hasSpec <- function(sampleDirs) {
  sapply(sampleDirs, function(d) {
    nf <- length(list.files(d, pattern = "\\.ProcSpec"))
    nf > 0
  })
}

# Remove lines from metadata that don't have matching specimens. This is ONLY
# intended for generating a testing data set.
trimMetadataForTesting <- function(dir, descr) {
  samDir <- file.path(dir, sampleDirectory(descr))
  good <- dir.exists(samDir)
  descr[good, ]
}

# Returns the name (without folder) of the zip file that contains the samples
zipFileForSpecimen <- function(descr) {
  if (ZIP_INTO == "Family") {
    paste0(descr$Family, ".zip")
  } else if (ZIP_INTO == "Genus") {
    paste0(paste(descr$Family, descr$Genus, sep = "_"), ".zip")
  } else if (ZIP_INTO == "Species") {
    paste0(paste(descr$Family, descr$Genus, descr$Species, sep = "_"), ".zip")
  }
}

# Update and generate metadata in other formats
genMetadata <- function(indir, zipDir = NULL, testingData = FALSE) {
  descr <- readDbMetadata(indir)

  if (testingData) {
    descr <- trimMetadataForTesting(indir, descr)
  }

  # Fix column names
  renameCol <- function(df, oldName, newName) {
    if (oldName %in% names(df))
      names(df)[names(df) == oldName] <- newName
    df
  }
  descr <- renameCol(descr, "Body", "Body.damage")
  descr <- renameCol(descr, "Forewing.left", "Forewing.dorsal.damage")
  descr <- renameCol(descr, "Forewing.right", "Forewing.ventral.damage")
  descr <- renameCol(descr, "Hindwing.left", "Hindwing.dorsal.damage")
  descr <- renameCol(descr, "Hindwing.right", "Hindwing.ventral.damage")

  # Get rid of Exclude column
  descr$Exclude <- NULL
  # Remove Process.ID column
  descr$Process.ID <- NULL
  # Update the Pinned column
  descr$Pinned <- ifelse(hasPinnedImages(file.path(indir, sampleDirectory(descr))), "y", "n")
  # Add a DNA column
  descr$DNA <- ifelse(hasDNA(file.path(indir, sampleDirectory(descr))), "y", "n")
  # Has a Spec file (if not already in metadata)
  if (!"Spectra" %in% names(descr)) {
    descr$Spectra <- ifelse(hasSpec(file.path(indir, sampleDirectory(descr))), "y", "n")
  }
  # Fix up the date format
  ## For day/month/year date format
  descr$Date <- strftime(dmy(descr$Date), "%d/%m/%Y")
  ## For ISO 8601 date format (year-month-day)
  ## descr$Date <- strftime(dmy(descr$Date), "%F")

  # Record the zip file that contains the sample in the repository
  descr$Repo.zipname <- zipFileForSpecimen(descr)

  # Write repo files
  utils::write.csv(descr, file = file.path(zipDir, paste0(METADATA_BASENAME, ".csv")), row.names = FALSE)
  openxlsx::write.xlsx(descr, file = file.path(zipDir, paste0(METADATA_BASENAME, ".xlsx")))
  jsonlite::write_json(descr, path = file.path(zipDir, paste0(METADATA_BASENAME, ".json")))

  ### Summary spreadsheet
  # Summarise dbs contents
  samp <- setNames(as.data.frame(table(descr$Binomial)), c("Species", "Specimens"))
  females <- setNames(as.data.frame(table(descr$Binomial[descr$Sex == "Female"])), c("Species", "Females"))
  males <- setNames(as.data.frame(table(descr$Binomial[descr$Sex == "Male"])), c("Species", "Males"))
  md <- merge(samp, merge(females, males, all = TRUE), all = TRUE)
  md[is.na(md)] <- 0

  # Combine with manually constructed species info, e.g. sexually dimorphic etc...
  man <- read.csv(file.path(indir, paste0(SUMMARY_BASENAME, ".csv")))
  # Sanity checks
  badSumSp <- !man$Species %in% unique(descr$Binomial)
  if (any(badSumSp)) {
    reportBad("Invalid species in summary file", man$Species[badSumSp])
    if (testingData) {
      cat("Ignoring error as this is only testing data\n")
    } else {
      stop("Bad summary file")
    }
  }
  md <- merge(md, man, all = TRUE)
  badMeta <- is.na(md$Dimorphic) | is.na(md$Iridescent)
  if (any(badMeta)) {
    reportBad("Missing summary data for species", md$Species[badMeta])
    if (testingData) {
      cat("Ignoring error as this is only testing data\n")
    } else {
      stop("Bad summary file")
    }
  }
  md <- md[, c("Family", "Species", "Females", "Males", "Specimens", "Dimorphic", "Iridescent")]
  md <- md[order(md$Family, md$Species), ]
  if (!testingData && !all.equal(md$Specimens, md$Females + md$Males)) { stop("Specimens != Females + Males") }

  # Write repo files
  utils::write.csv(md, file = file.path(zipDir, paste0(SUMMARY_BASENAME, ".csv")), row.names = FALSE)
  openxlsx::write.xlsx(md, file = file.path(zipDir, paste0(SUMMARY_BASENAME, ".xlsx")))
  jsonlite::write_json(md, path = file.path(zipDir, paste0(SUMMARY_BASENAME, ".json")))

  # Copy the README
  file.copy(file.path(indir, "README.txt"), zipDir)

  # Copy the colour standard spectral reflectance files
  file.copy(list.files(indir, pattern = "standard.*\\.ProcSpec", full.names = TRUE), zipDir)

  descr
}


# Create repo structure, metadata files and zip files
createZippedDb <- function(indir, zipDir, metadataOnly = FALSE) {

  # Copy (and generate) meta data files
  if (!dir.exists(zipDir)) {
    dir.create(zipDir, recursive = TRUE)
  }
  descr <- genMetadata(indir, zipDir, FOR_TESTING_ONLY)

  if (!metadataOnly) {

    # Options for zip files are Family, Genus or Species
    if (ZIP_INTO == "Family") {
      # Zip into families
      srcDir <- unique(descr$Family)
      zipSubset <- FALSE   # All files in the source folder are stored in the zip file
    } else if (ZIP_INTO == "Genus") {
      genera <- unique(descr[, c("Family", "Genus")])
      # Samples are not grouped into genus folders, so we obtain the data from the family folders
      srcDir <- genera$Family
      # Define a pattern to match the names of the files we want in this zip file
      zipSubset <- paste(genera$Family, genera$Genus, sep = "/")
      zipSubset <- paste0("^", zipSubset, "_")
    } else if (ZIP_INTO == "Species") {
      # Zip into species
      species <- unique(descr[, c("Family", "Genus", "Species")])
      srcDir <- speciesDirectory(species)
      zipSubset <- FALSE   # All files in the source folder are stored in the zip file
    }

    # Get the list of all zip files
    zipName <- unique(zipFileForSpecimen(descr))
    zipPath <- normalizePath(file.path(zipDir, zipName), mustWork = FALSE)

    origDir <- getwd()
    on.exit({ setwd(origDir) }, add = TRUE)
    # Set working directory to the parent of the Family folder so that we get
    # the relative paths in the zip file that we want
    setwd(indir)

    pb <- JBuildProgressBar(progressBar = "win", numItems = length(zipName), title = "")
    skippedDirs <- 0
    for (fi in seq_along(zipName)) {
      if (!dir.exists(srcDir[fi])) {
        skippedDirs <- skippedDirs + 1
      } else {
        files <- list.files(srcDir[fi], recursive = TRUE, full.names = TRUE)
        # If we are zipping to family or species, it's simple because we are
        # just zipping up an entire folder. When zipping to genus, it's more
        # complicated because we want a subset of the folders
        if (!isFALSE(zipSubset)) {
          files <- grep(zipSubset[fi], files, value = TRUE)
        }
        zip <- zipPath[fi]
        # Delete old zip file
        unlink(zip)
        x <- utils::zip(zip, files)
        if (x != 0) stop(sprintf("Zip failed with status %d", x))
        pb()
      }
    }
    if (skippedDirs > 0) {
      cat(sprintf("Skipped %d species folders because they don't exist\n", skippedDirs))
    }
    pb(close = TRUE, printElapsed = TRUE)
  }
}


#######################################################

# Check the database to be packed up
checkOzButtUnpacked(DBDIR)

# Check species and family names against the Australian faunal directory
# checkSpecies(DBDIR)

# Create zip files, including metadata in other formats
createZippedDb(DBDIR, REPODIR, METADATA_ONLY)


