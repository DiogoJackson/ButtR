# Database structure (unpacked)
# Oz_butterflies.csv
# Oz_butterflies.xlsx
# Oz_butterflies.json
# Folder for each family, which contains
# Folder for each species, which contains
# Folder for each specimen
#
# Specimen folder = 1, 2 ... 13, etc...
# Family/species/specimen
# Within each specimen folder:
# Always contains
# "<ID>RGB.arw"
# "<ID>UV.arw"
# May contain
# Sequence file
# 2 x linear TIFFs (with corrected specimen IDs), named as for raw files
# 1 x TIFF with spec locations highlighted (name TBD)
# Spec files ("<ID><species initials><a|n><spot ID>.rspec") (a|n = angle or normal))
# CSV equivalent of all spec files

# In Dryad:
# Oz_butterflies.csv
# Oz_butterflies.xlsx
# Oz_butterflies.json
# Lots of species zip files
#

# Download one or more files from the repository
downloadFiles <- function(files, subset, destDir) {
  # Download them one at a time
  for (idx in subset) {
    # Define the path where the file will be saved locally
    destfile <- file.path(destDir, files$file[idx])

    # Download the spreadsheet from the URL and save it in the specified path
    utils::download.file(url = files$url[idx], destfile = destfile, mode = "wb", quiet = TRUE)
  }
}

#' Downloads all or part of the Oz butterflies database to a local folder
#'
#' Simplifies downloading the Oz butterflies database to a local folder. Since
#' the database is quite large, download times are long and the database
#' requires substantial local storage space. If the entire database is nto
#' needed, then this function saves time and local storage space by only
#' downloading the required parts of the database.
#'
#' @param species Optional vector of binomial names of species of interest. If
#'   specified, only species from this list will be included in the local
#'   database.
#' @param genus If specified, only specimens from this genus will be installed.
#' @param family If specified, only specimens from this family will be
#'   installed.
#' @param sex If specified, only specimens of this sex (\code{"male"},
#'   \code{"female"} or  \code{"unknown"}) will be installed.
#' @param genus If specified, only specimens from these genera will be
#'   installed.
#' @param year If specified, only specimens collected during these years will be
#'   installed (options are 2022 or 2023).
#' @param location If specified, only specimens collected at these locations
#'   will be installed (options are \code{"Brisbane"}, \code{"Cairns"} and
#'   \code{"Sydney"}).
#' @param reflectance If specified, only specimens with the specified
#'   reflectance will be installed (\code{"yes"} or \code{"no"}).
#' @param sampleIDs If specified, only specimens with the specified IDs will be
#'   installed.
#' @param download_images Specifies whether \code{"raw"} and/or \code{"jpeg"}
#'   images should be downloaded. Only images with the specified type(s) will be
#'   downloaded.
#' @param db_folder Path of folder that will contain the downloaded database.
#'
#' @returns Path of the downloaded folder (invisibly).
#'
#' @export
get_species <- function(species = NULL,
                        genus = NULL,
                        family = NULL,
                        sex = NULL,
                        year = NULL,
                        location = NULL,
                        reflectance = NULL,
                        sampleIDs = NULL,
                        download_images = c("raw", "jpeg"),
                        db_folder = "Oz_butterflies") {

  download_images <- match.arg(download_images, several.ok = TRUE)

  # Check if the "db_folder" directory (where the data will be stored) exists.
  # If it doesn't exist, create the folder.
  if (!dir.exists(db_folder)) {
    dir.create(db_folder, recursive = TRUE)
  }

  # List all files in the database
  files <- ListDbsFiles()

  # Download the metadata spreadsheet in all formats
  metadata <- grep("Oz_butterflies\\.", files$file)
  if (length(metadata) == 0) {
    stop("Unable to locate Oz butterflies metadata file in repository")
  }
  downloadFiles(files, metadata, db_folder)

  # Identify the local spreadsheet file path
  spread_sheet <- file.path(db_folder, "Oz_butterflies.csv")

  # Read metadata
  meta_data <- utils::read.csv(spread_sheet)

  # Create a new column "full_species" that combines the "genus" and "species" columns into a full species name
  meta_data$full_species <- paste(meta_data$Genus, meta_data$Species)

  # Create a new column "zipname" that combines "family", genus", "species" into a single name, separated by "_"
  meta_data$zipname <- paste(meta_data$Family, meta_data$Genus, meta_data$Species, sep = "_")

  # Add the ".zip" extension to the end of the file names in the "zipname" column
  meta_data$zipname <- paste0(meta_data$zipname, ".zip")

  # Start by selecting all rows (TRUE will be repeated nrow times)
  rows <- rep(TRUE, nrow(meta_data))

  # If the user has specified a family (or more), this will filter the 'meta_data'
  # to include only the rows where the 'Family' column matches any specified family names.
  # The '&' operator is used to combine this condition with any previously applied,
  # ensuring that the final set of rows meets all specified criteria.
  if (length(family) > 0) {
    rows <- rows & tolower(meta_data$Family) %in% tolower(family)
  }

  # If genus is specified, update the filtering
  if (length(genus) > 0) {
    rows <- rows & tolower(meta_data$genus) %in% tolower(genus)
  }

  # If species is specified, update the filtering
  if (length(species) > 0) {
    badSpecies <- !tolower(species) %in% unique(tolower(meta_data$full_species))
    if (any(badSpecies)) {
      stop(sprintf("The following requested species do not exist in the Oz butterflies database: %s",
                   paste(species[badSpecies], collapse = ", ")))
    }
    rows <- rows & tolower(meta_data$full_species) %in% tolower(species)
  }

  # If location is specified, update the filtering
  if (length(location) > 0) {
    rows <- rows & meta_data$location %in% location
  }

  # If spectra is specified, update the filtering
  if (length(reflectance) > 0) {
    rows <- rows & meta_data$reflectance %in% reflectance
  }

  # If sex is specified, update the filtering
  if (length(sex) > 0) {
    rows <- rows & tolower(meta_data$sex) %in% tolower(sex)
  }

  # If year is specified, update the filtering
  if (length(year) > 0) {
    rows <- rows & meta_data$year %in% year
  }

  # If sample ID is specified, update the filtering
  if (length(sampleIDs) > 0) {
    rows <- rows & meta_data$ID %in% sampleIDs
  }

  # Get the unique zip file names that match the filtered criteria
  zips <- unique(meta_data$zipname[rows])

  # Create a temporary directory to store zip files
  tempdir <- tempdir()

  # Iterate over each zip file
  for (zip in zips) {
    # Define the path to be saved temporarily
    zip_path <- file.path(tempdir, zip)

    # Get the URL of the corresponding zip file
    if (!zip %in% files$file) {
      stop(sprintf("Internal error: zip file %s not found in repository", zip))
    }
    url <- files$url[files$file == zip]

    # Download the zip file from the URL
    utils::download.file(url = url, destfile = zip_path, mode = "wb", quiet = TRUE)

    # List files inside the zip archive without extracting them
    zip_content <- utils::unzip(zip_path, list = TRUE)

    # Select files in this zip that match the filtered sample IDs.
    # Assume sample folder is sample ID
    samplesInZip <- basename(dirname(zip_content$Name))
    selected_files <- zip_content$Name[samplesInZip %in% meta_data$ID[rows]]

    # If not all image files were requested, filter them out
    if (!"raw" %in% download_images) {
      selected_files <- selected_files[grep("\\.arw", selected_files, invert = TRUE, ignore.case = TRUE)]
    }
    if (!"jpeg" %in% download_images) {
      selected_files <- selected_files[grep("\\.jpg", selected_files, invert = TRUE, ignore.case = TRUE)]
    }

    # If there are matching files, extract only those files
    if (length(selected_files) > 0) {
      utils::unzip(zip_path, files = selected_files, exdir = db_folder)
    }

    # Delete the zip file so that disk space use is minimised
    unlink(zip_path)
  }

  invisible(normalizePath(db_folder))
}
