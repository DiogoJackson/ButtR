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

# Checks whether requested values for some parameter exist in the database. Throws an error if not
checkValuesInSet <- function(what1, whatn, requested, available) {
  badVals <- !tolower(requested) %in% unique(tolower(available))
  if (any(badVals)) {
    if (sum(badVals) == 1)
      stop(sprintf("The following requested %s does not exist in the Oz butterflies database: %s",
                   what1, requested[badVals]))
    else
      stop(sprintf("The following requested %s do not exist in the Oz butterflies database: %s",
                 whatn, paste(requested[badVals], collapse = ", ")))
  }
}

#' @title ButtR - Oz butterflies database
#' @description The Oz butterflies database contains reflectance spectra and images of Australian butterflies.
#' Downloads all or part of the Oz butterflies database to a local folder.
#'
#' Simplifies downloading the Oz butterflies database to a local folder. Since
#' the database is quite large, download times are long and the database
#' requires substantial local storage space. If the entire database is not
#' needed, then this function saves time and local storage space by only
#' downloading the required parts of the database.
#'
#' The metadata files in the database (Oz_butterflies.csv, Oz_butterflies.xslx
#' and Oz_butterflies.json) always describe the entire database, regardless of
#' whether the entire database or a subset is installed locally.
#'
#' @param species Optional vector of binomial names of species of interest. If
#'   specified, only species from this list will be included in the local
#'   database.
#' @param genus If specified, only specimens from this genus will be installed.
#' @param family If specified, only specimens from this family will be
#'   installed.
#' @param sex If specified, only specimens of this sex (\code{"male"},
#'   \code{"female"} or  \code{"unknown"}) will be installed.
#' @param year If specified, only specimens collected during these years will be
#'   installed (options are 2022 or 2023).
#' @param site If specified, only specimens collected at these sites
#'   will be installed.
#' @param reflectance If specified, only specimens with the specified
#'   reflectance will be installed (\code{"yes"} or \code{"no"}).
#' @param sampleIDs If specified, only specimens with the specified IDs will be
#'   installed.
#' @param download_images Specifies whether \code{"raw"} and/or \code{"jpeg"}
#'   images should be downloaded. Only images with the specified type(s) will be
#'   downloaded.
#' @param db_folder Path of folder that will contain the downloaded database.
#'
#' @examples
#' \dontrun{
#' # Download the full Oz Butterflies Database
#' get_Oz_butterflies()
#'
#' # Get data only for Delias aganippe
#' get_Oz_butterflies(species = "Delias aganippe")
#'
#' # Get data for all species of the genus Delias
#' get_Oz_butterflies(genus = "Delias")
#'
#' # Get all species within the Nymphalidae family
#' get_Oz_butterflies(family = "Nymphalidae")
#'
#' # Get data with multiple filters
#' get_Oz_butterflies(species = c("Delias aganippe", "Delias mysis"))
#' }
#'
#' @export
get_Oz_butterflies <- function(species = NULL,
                        genus = NULL,
                        family = NULL,
                        sex = NULL,
                        year = NULL,
                        site = NULL,
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
    stop("Internal error: Unable to locate Oz butterflies metadata file in repository")
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
    checkValuesInSet("family", "families", tolower(family), tolower(meta_data$Family))
    rows <- rows & tolower(meta_data$Family) %in% tolower(family)
  }

  # If genus is specified, update the filtering
  if (length(genus) > 0) {
    checkValuesInSet("genus", "genus", tolower(genus), tolower(meta_data$Genus))
    rows <- rows & tolower(meta_data$Genus) %in% tolower(genus)
  }

  # If species is specified, update the filtering
  if (length(species) > 0) {
    checkValuesInSet("species", "species", tolower(species), tolower(meta_data$full_species))
    rows <- rows & tolower(meta_data$full_species) %in% tolower(species)
  }

  # If site is specified, update the filtering
  if (length(site) > 0) {
    checkValuesInSet("site", "sites", tolower(site), tolower(meta_data$Site))
    rows <- rows & meta_data$Site %in% site
  }

  # If spectra is specified, update the filtering
  if (length(reflectance) > 0) {
    checkValuesInSet("reflectance", "reflectances", tolower(reflectance), tolower(meta_data$Speced))
    rows <- rows & meta_data$Speced %in% reflectance
  }

  # If sex is specified, update the filtering
  if (length(sex) > 0) {
    checkValuesInSet("sex", "sexes", tolower(sex), tolower(meta_data$Sex))
    rows <- rows & tolower(meta_data$Sex) %in% tolower(sex)
  }

  # Filtrando apenas os anos especificados
  if (length(year) > 0) {
    meta_data$Date <- substr(meta_data$Date, 7, 10)  # extract year from date
    rows <- rows & meta_data$Date %in% as.character(year)  # string
  }

  # If sample ID is specified, update the filtering
  if (length(sampleIDs) > 0) {
    checkValuesInSet("sampleIDs", "sampleID", tolower(sampleIDs), tolower(meta_data$ID))
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
      stop(sprintf("Internal error: zip file %s not found in repository: files in repo are %s", zip, paste(files$file, collapse = ", ")))
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
