library(readxl)

# Database structure (unpacked)
# Oz_butterflies.xlsx
# Oz_butterflies.csv (TBD)
# Oz_butterflies.json (TBD)
# Folder for each family, which contains
# Folder for each species, which contains
# Folder for each specimen
#
# Specimen folder
# TO DO Name of specimen folder?
# Family/species/specimen
# Within each specimen folder:
# Always contains
# "<ID>(RGB).arw"
# "<ID>(UV).arw"
# May contain
# Sequence file
# 2 x linear TIFFs (with corrected specimen IDs), named as for raw files
# 1 x TIFF with spec locations highlighted (name TBD)
# Spec files ("<ID><species initials><a|n><spot ID>.rspec") (a|n = angle or normal))
# CSV equivalent of all spec files

# In Dryad:
# Oz_butterflies.xlsx
# Oz_butterflies.csv (TBD)
# Oz_butterflies.json (TBD)
# Lots of species zip files
#
#' @export
get_species <- function(species = NULL,
                        genus = NULL,
                        family = NULL,
                        location = c("Sydney", "Brisbane", "Cairns"),
                        db_folder = "Oz_butterflies") {

  # Check if the "db_folder" directory (where the data will be stored) exists.
  # If it doesn't exist, create the folder.
  if (!dir.exists(db_folder)) {
    dir.create(db_folder)
  }

  # List all local files present in the specified folder (in this case, "data_buttr")
  files <- listLocalFiles("C:\\Users\\Diogo Silva\\Desktop\\data_buttr")

  # Filter the URL of the "Oz_butterflies.xlsx" file from the listed files
  url_oz <- files$url[files$file == "Oz_butterflies.xlsx"]

  # Define the path where the spreadsheet will be saved locally
  spread_sheet <- file.path(db_folder, "Oz_butterflies.xlsx")

  # Download the spreadsheet from the URL and save it in the specified path
  # utils::download.file(url = url_oz, destfile = spread_sheet, mode = "wb")

  # Path to the local folder where files are stored
  local_folder <- "C:\\Users\\Diogo Silva\\Desktop\\data_buttr"
  spread_sheet <- file.path(local_folder, "Oz_butterflies.xlsx")
  file.copy(spread_sheet, file.path(db_folder, "Oz_butterflies.xlsx"), overwrite = TRUE)

  # Testing in the PC (without download)
  # Read the data from desktop
  meta_data <- readxl::read_excel(spread_sheet,
                                  skip = 1)

  # Create a new column "full_species" that combines the "genus" and "species" columns into a full species name
  #-----------------I deleted the family name, because I dont think we need it ---------------------------
  meta_data$full_species <- paste(meta_data$genus, meta_data$species)

  # Create a new column "zipname" that combines "genus", "species" into a single name, separated by "_"

  meta_data$zipname <- paste(meta_data$genus, meta_data$species, sep = "_")

  # Add the ".zip" extension to the end of the file names in the "zipname" column
  meta_data$zipname <- paste0(meta_data$zipname, ".zip")

  # If no species or family is specified, select all rows (TRUE will be repeated nrow times)
  rows <- rep(TRUE, nrow(meta_data))


  # Simulating parameters settings
  # family <- NULL
  # genus <- NULL
  # species <- NULL
  # location <- c("Sydney", "Brisbane", "Cairns")

  # If a family is specified, update the filtering
  if (length(family) > 0) {
    rows <- rows & meta_data$Family %in% family
  }

  # If genus is specified, update the filtering
  if (length(genus) > 0) {
    rows <- rows & meta_data$genus %in% genus
  }

  # If species is specified, update the filtering
  if (length(species) > 0) {
    rows <- rows & meta_data$full_species %in% species
  }

  # If location is specified, update the filtering
  if (length(location) > 0) {
    rows <- rows & meta_data$location %in% location
  }

  # Get the unique zip file names that match the filtered criteria
  zips <- unique(meta_data$zipname[rows])

  # Create a temporary directory to store zip files
  tempdir <- tempdir()

  # Iterate over each zip file
  for (zip in zips) {
    zip_path <- file.path(local_folder, zip)

    # Check if the zip file exists locally before proceeding
    if (file.exists(zip_path)) {

      # List files inside the zip archive without extracting them
      zip_content <- utils::unzip(zip_path, list = TRUE)

      # Extract IDs from the file names and filter based on location
      file_ids <- gsub("\\D", "", zip_content$Name)
      valid_ids <- unique(meta_data$ID[meta_data$location %in% location])
      selected_files <- zip_content$Name[file_ids %in% valid_ids]

      # If there are matching files, extract only those files
      if (length(selected_files) > 0) {
        utils::unzip(zip_path, files = selected_files, exdir = file.path(db_folder, zip))

      }
    } else {
      warning(paste("Zip file not found locally:", zip))
    }
  }
}

# For each selected zip file, downloading and extracting the files
#   for (zip in zips) {
#
#     # Define the path to be saved temporarily
#      zip_path <- file.path(tempdir, zip)
#
#     # Get the URL of the corresponding zip file
#      url <- files$url[files$file == zip]
#
#     # Download the zip file from the URL
#     # utils::download.file(url = url, destfile = zip_path, mode = "wb")
#
#     # List the files without extracting them
#     zip_content <- utils::unzip(zip_path, list = TRUE)
#
#     # Extract IDs from the file names (assuming file names are like "specimen01", "specimen21"...)
#     file_ids <- gsub("\\D", "", zip_content$Name)  # Remove non-digit characters to get IDs
#
#     # Filtering the IDs corresponding to the specified locations
#     ids_to_extract <- unique(meta_data$ID[meta_data$location %in% location])
#
#     # Select the files whose IDs are in the list to extract
#     selected_files <- zip_content$Name[file_ids %in% ids_to_extract]
#
#     # Extract only selected files
#     if (length(selected_files) > 0) {
#       utils::unzip(zip_path, files = selected_files, exdir = file.path(db_folder, zip))
#
#       message(paste("Files have been successfully downloaded to the folder:", db_folder))
#
#     }
#   }
# }

