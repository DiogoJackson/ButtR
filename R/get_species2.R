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
                        db_folder = "Oz_butterflies",
                        family = NULL,
                        location = c("Sydney", "Brisbane", "Cairns")) {

  # Check if the "db_folder" directory (where the data will be stored) exists.
  # If it doesn't exist, create the folder.
  if (!dir.exists(db_folder)) {
    dir.create(db_folder)
  }

  # List all local files present in the specified folder (in this case, "data_buttr")
  files <- listLocalFiles("C:\\Users\\Diogo Silva\\Desktop\\data_buttr")

  # Filter the URL of the "Oz_butterflies.xlsx" file from the listed files
  url <- files$url[files$file == "Oz_butterflies.xlsx"]

  # Define the path where the spreadsheet will be saved locally
  spread_sheet <- file.path(db_folder, "Oz_butterflies.xlsx")

  # Download the spreadsheet from the URL and save it in the specified path
  utils::download.file(url = url, destfile = spread_sheet, mode = "wb")

  # Read the data from the spreadsheet, skipping the first row
  meta_data <- readxl::read_excel(spread_sheet, skip = 1)

  # Create a new column "full_species" that combines the "genus" and "species" columns into a full species name
  #-----------------I deleted the family name, because I dont think we need it ---------------------------
  meta_data$full_species <- paste(meta_data$genus, meta_data$species)

  # Create a new column "zipname" that combines "genus", "species" into a single name, separated by "_"

  meta_data$zipname <- paste(meta_data$genus, meta_data$species, sep = "_")

  # Add the ".zip" extension to the end of the file names in the "zipname" column
  meta_data$zipname <- paste0(meta_data$zipname, ".zip")

  # If no species or family is specified, select all rows (TRUE will be repeated nrow times)
  rows <- rep(TRUE, nrow(meta_data))

  # If a family is specified, update the filtering
  if (length(family) > 0) {
    rows <- rows & meta_data$Family %in% family
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

  # Create a temporary directory to store the downloaded zip files
  tempdir <- tempdir()

  # Iterate through each selected zip file, downloading and extracting the files
  for (zip in zips) {
    # Define the path where the zip file will be saved temporarily
    zip_path <- file.path(tempdir, zip)

    # Get the URL of the corresponding zip file
    url <- files$url[files$file == zip]

    # Download the zip file from the URL and save it to the temporary path
    utils::download.file(url = url, destfile = zip_path, mode = "wb")

    # List the files inside the zip archive without extracting them
    zip_content <- utils::unzip(zip_path, list = TRUE)

    # Extract IDs from the file names (assuming file names are like "specimen01", "specimen21", etc.)
    file_ids <- gsub("\\D", "", zip_content$Name)  # Remove non-digit characters to get IDs

    # Find the IDs corresponding to the specified locations
    valid_ids <- unique(meta_data$ID[meta_data$location %in% location])

    # Select the files whose IDs are in the valid_ids list
    selected_files <- zip_content$Name[file_ids %in% valid_ids]

    # If there are matching files, extract only those files
    if (length(selected_files) > 0) {
      utils::unzip(zip_path, files = selected_files, exdir = file.path(db_folder, zip))
    }
  }
}

